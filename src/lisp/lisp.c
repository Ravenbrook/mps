/*
 * main() entry point for a stand alone lisp image.
 *
 * $Header: /project/cmucl/cvsroot/src/lisp/lisp.c,v 1.50 2005/09/15 18:26:52 rtoy Exp $
 *
 */

#include <stdio.h>
#include <sys/types.h>
#include <stdlib.h>
#include <limits.h>
#include <sys/file.h>
#include <sys/param.h>
#include <sys/stat.h>

#include "signal.h"

#include "lisp.h"
#include "internals.h"
#include "alloc.h"
#include "vars.h"
#include "globals.h"
#include "os.h"
#include "interrupt.h"
#include "arch.h"
#include "gc.h"
#include "monitor.h"
#include "validate.h"
#include "interr.h"
#if defined GENCGC
#include "gencgc.h"
#endif
#include "core.h"
#include "save.h"
#include "lispregs.h"



/* SIGINT handler that invokes the monitor. */

static void
sigint_handler(HANDLER_ARGS)
{
#if ( defined( __linux__ ) && ( defined( i386 ) || defined ( __x86_64 ) ) )
    GET_CONTEXT
#endif
	SAVE_CONTEXT();

    printf("\nSIGINT hit at 0x%08lX\n", (unsigned long) SC_PC(context));
    ldb_monitor();
}

/* Not static, because we want to be able to call it from lisp land. */
void
sigint_init(void)
{
    install_handler(SIGINT, sigint_handler);
}


/* Noise to convert argv and envp into lists. */

static lispobj
alloc_str_list(char *list[])
{
    lispobj result, newcons;
    struct cons *ptr;

    if (*list == NULL)
	result = NIL;
    else {
	result = newcons = alloc_cons(alloc_string(*list++), NIL);

	while (*list != NULL) {
	    ptr = (struct cons *) PTR(newcons);
	    newcons = alloc_cons(alloc_string(*list++), NIL);
	    ptr->cdr = newcons;
	}
    }

    return result;
}

/* Default paths for CMUCLLIB */
static char *cmucllib_search_list[] = {
    "./.",
    "./../lib/cmucl/lib",
    "./../lib",
    "/usr/local/lib/cmucl/lib",
    "/usr/lib/cmucl",
    NULL
};


/* Set this to see how we're doing our search */
static int debug_lisp_search = FALSE;

/*
 * Define this to get some debugging printfs for searching for the
 * lisp core file.  Sometimes needed because you can't debug this with
 * gdb which always seems to set argv[0] to the full pathname.
 */

/* #define DEBUG_LISP_SEARCH */

/*
 * From the current location of the lisp executable, create a suitable
 * default for CMUCLLIB
 */
char *
default_cmucllib(const char *argv0arg)
{
    char *p;
    char *defpath;
    char *cwd;
    char *argv0_dir = strdup(argv0arg);

    /*
     * From argv[0], create the appropriate directory by lopping off the
     * executable name
     */

    p = strrchr(argv0_dir, '/');
    if (p == NULL) {
	*argv0_dir = '\0';
    } else if (p != argv0_dir) {
	*p = '\0';
    }

    /*
     * Create the full pathname of the directory containing the
     * executable.  argv[0] can be an absolute or relative path.
     */
    if (debug_lisp_search) {
	fprintf(stderr, "argv[0] = %s\n", argv0arg);
	fprintf(stderr, "argv_dir = %s\n", argv0_dir);
    }


    if (argv0_dir[0] == '/') {
	cwd = malloc(strlen(argv0_dir) + 2);
	strcpy(cwd, argv0_dir);
	strcat(cwd, "/");
	if (debug_lisp_search) {
	    fprintf(stderr, "absolute path, argv[0] = %s\n", cwd);
	}

    } else if (*argv0_dir != '\0') {
	/*
	 * argv[0] is a relative path.  Get the current directory and
	 * append argv[0], after stripping off the executable name.
	 */
	cwd = malloc(MAXPATHLEN + strlen(argv0_dir) + 100);
	getcwd(cwd, MAXPATHLEN);
	strcat(cwd, "/");
	if (*argv0_dir != '\0') {
	    strcat(cwd, argv0_dir);
	    strcat(cwd, "/");
	}
	if (debug_lisp_search) {
	    fprintf(stderr, "relative path, argv[0] = %s\n", cwd);
	}
    } else {
	/*
	 * argv[0] is someplace on the user's PATH
	 *
	 */
	char *path = getenv("PATH");
	char *p1, *p2 = NULL;
	struct stat buf;

	if (debug_lisp_search) {
	    fprintf(stderr, "User's PATH = %s\n", path ? path : "<NULL>");
	}

	cwd = malloc(MAXPATHLEN + strlen(argv0arg) + 100);
	cwd[0] = '\0';

	if (path) {

	    if (p == NULL) {
		p = argv0arg;
	    }

	    for (p1 = path; *p1 != '\0'; p1 = p2) {
		p2 = strchr(p1, ':');
		if (p2 == NULL)
		    p2 = p1 + strlen(p1);
		strncpy(cwd, p1, p2 - p1);
		cwd[p2 - p1] = '/';
		cwd[p2 - p1 + 1] = '\0';
		strcpy(cwd + (p2 - p1 + 1), p);

		if (debug_lisp_search) {
		    fprintf(stderr, "User's PATH, trying %s\n", cwd);
		}

		if (stat(cwd, &buf) == 0) {

		    if (debug_lisp_search) {
			fprintf(stderr, "User's PATH, found %s\n", cwd);
		    }
		    if (access(cwd, X_OK) == 0) {
			break;
		    } else {
			if (debug_lisp_search) {
			    fprintf(stderr,
				    " But not executable.  Continuing...\n");
			}
		    }

		}

		if (*p2 == ':') {
		    p2++;
		}

	    }
	    if ((p1 == p2) || (p2 == NULL)) {
		cwd[0] = '\0';
	    } else {
		cwd[p2 - p1 + 1] = '\0';
	    }
	    if (debug_lisp_search) {
		fprintf(stderr, "User's PATH, Final cwd %s\n", cwd);
	    }

	}
    }

    /* Create the appropriate value for CMUCLLIB */

    {
	char **ptr;
	int total_len;
	int cwd_len;

	/* First figure out how much space we need */

	total_len = 0;
	cwd_len = strlen(cwd);

	ptr = cmucllib_search_list;

	while (*ptr != NULL) {
	    /* Plus 2 for the ":" and "/" we need to add */
	    total_len += strlen(*ptr) + cwd_len + 2;
	    ++ptr;
	}

	/* Create the colon separated list of directories */

	defpath = malloc(total_len + 1);
	*defpath = '\0';

	ptr = cmucllib_search_list;
	while (*ptr != NULL) {
	    if (*ptr[0] != '/') {
		strcat(defpath, cwd);
	    }

	    strcat(defpath, *ptr);

	    if (ptr[1] != NULL) {
		strcat(defpath, ":");
	    }

	    ++ptr;
	}

	if (strlen(defpath) > total_len) {
	    abort();
	}
    }

    free(argv0_dir);
    free(cwd);

    return defpath;
}

/*
 * Search the a core file with the name given by default_core in the
 * colon-separated list of directories given by lib.
 *
 * Return the full path, if found, or NULL if not.
 */

char *
search_core(const char *lib, const char *default_core)
{
    char *buf;
    char *dst;

    /*
     * A buffer that's large enough to hold lib, default_core, a
     * slash, and a the string terminator
     */
    buf = malloc(strlen(lib) + strlen(default_core) + 2);

    do {
	dst = buf;
	/*
	 * Extract out everything to the first colon, then append a
	 * "/" and the core name.  See if the file exists.
	 */
	while (*lib != '\0' && *lib != ':')
	    *dst++ = *lib++;
	if (dst != buf && dst[-1] != '/')
	    *dst++ = '/';
	strcpy(dst, default_core);
	/* If it exists, we are done! */

	if (debug_lisp_search) {
	    fprintf(stderr, "Looking at `%s'\n", buf);
	}

	if (access(buf, R_OK) == 0) {
	    if (debug_lisp_search) {
		fprintf(stderr, "Found it!\n");
	    }

	    return buf;
	} else {
	    if (debug_lisp_search) {
		fprintf(stderr, "Found it, but we can't read it!\n");
	    }
	}
    } while (*lib++ == ':');

    free(buf);
    return NULL;
}

/*
 * Given the path to a core file, prepend the absolute location of the
 * core file to the lib path.
 *
 * Return the new lib path.
 */
char *
prepend_core_path(char *lib, char *corefile)
{
    char cwd[MAXPATHLEN];
    char *path;
    char *result;
    char *sep;

    if (*corefile == '/') {
	path = strdup(corefile);
    } else {
	/*
	 * We have a relative path for the corefile.  Prepend our current
	 * directory to get the full path.
	 */
	getcwd(cwd, MAXPATHLEN);
	path = malloc(MAXPATHLEN + strlen(corefile) + 2);
	strcpy(path, cwd);
	strcat(path, "/");
	strcat(path, corefile);
    }

    /*
     * Now remove the name portion by finding the last slash.
     */
    sep = strrchr(path, '/');
    if (sep != NULL) {
	*sep = '\0';
    }

    result = malloc(strlen(path) + strlen(lib) + 2);
    strcpy(result, path);
    strcat(result, ":");
    strcat(result, lib);

    free(path);
    return result;
}


/*
  The symbol builtin_image_flag is used globally as a flag to indicate
  whether the executable contains the lisp image.  Note that we use
  the linker to set the value of the symbol.  But the symbol is an
  address, not a variable value.  So for this to work as a flag, it
  must end up pointing to a valid place in memory or we'll get a bus
  error or segmentation violation when we check it.  If the lisp image
  is built in, we'll set this symbol to point to the beginning of the
  process.

  We also use the linker to set initial_function_addr so that if the
  lisp core is built in, taking the address of initial_function_addr
  will give the address of the initial function.
*/

int builtin_image_flag = 0;
long initial_function_addr = 0;

/* And here be main. */

int
main(int argc, char *argv[], char *envp[])
{
    char *arg, **argptr;
    char *core = NULL;
    char *default_core;
    char *lib = NULL;
    char *cmucllib = NULL;

    boolean monitor;
    lispobj initial_function = 0;

    if (builtin_image_flag != 0)
	initial_function = (lispobj) & initial_function_addr;

#if defined(SVR4) || defined(__linux__)
    tzset();
#endif

    set_lossage_handler(ldb_monitor);

    monitor = FALSE;

#ifdef DEFAULT_DYNAMIC_SPACE_SIZE
    dynamic_space_size = DEFAULT_DYNAMIC_SPACE_SIZE;
#else
    dynamic_space_size = DYNAMIC_SPACE_SIZE;
#endif

    argptr = argv;
    while ((arg = *++argptr) != NULL) {
	if (strcmp(arg, "-core") == 0) {
	    if (builtin_image_flag) {
		fprintf(stderr,
			"Cannot specify core file in executable image --- sorry about that.\n");
		exit(1);
	    }
	    if (core != NULL) {
		fprintf(stderr, "can only specify one core file.\n");
		exit(1);
	    }
	    core = *++argptr;
	    if (core == NULL) {
		fprintf(stderr,
			"-core must be followed by the name of the core file to use.\n");
		exit(1);
	    }
	} else if (strcmp(arg, "-lib") == 0) {
	    lib = *++argptr;
	    if (lib == NULL) {
		fprintf(stderr,
			"-lib must be followed by a string denoting the CMUCL library path.\n");
		exit(1);
	    }
	} else if (strcmp(arg, "-dynamic-space-size") == 0) {
	    char *str;

	    str = *++argptr;
	    if (str == NULL) {
		fprintf(stderr,
			"-dynamic-space-size must be followed by the size to use in MBytes.\n");
		exit(1);
	    }
#ifndef sparc
	    dynamic_space_size = atoi(str) * 1024 * 1024;
#else
	    {
		int val;

		/*
		 * Martin Rydstrom says core sizes that aren't a
		 * multiple of 8 MB eventually causes GC lossage with
		 * gencgc on Solaris 10.  No one seems to understand why
		 * that is, but it is.  So here we enforce the 8 MB
		 * boundary by rounding up the size.  We print a warning
		 * message if we do have to round.
		 *
		 * We do this for all versions, since it doesn't hurt
		 * other versions of Solaris.
		 */
		val = atoi(str);
		dynamic_space_size = (val + 7) & ~7;

		if (val != dynamic_space_size) {
		    fprintf(stderr,
			    "Note:  Rounding dynamic-space-size from %d MB to %d MB\n",
			    val, dynamic_space_size);
		}
		dynamic_space_size *= 1024 * 1024;
	    }
#endif
	    if (dynamic_space_size > DYNAMIC_SPACE_SIZE) {
		fprintf(stderr,
			"-dynamic-space-size must be no greater than %d MBytes.\n",
			DYNAMIC_SPACE_SIZE / (1024 * 1024));
		exit(1);
	    }
	} else if (strcmp(arg, "-monitor") == 0) {
	    monitor = TRUE;
	} else if (strcmp(arg, "-debug-lisp-search") == 0) {
	    debug_lisp_search = TRUE;
	}
    }

    default_core = arch_init();
    if (default_core == NULL)
	default_core = "lisp.core";

    os_init();
#if defined(__FreeBSD__) || defined(__linux__)
    if (builtin_image_flag != 0)
	map_core_sections(argv[0]);
#endif
    validate();
    gc_init();

    /* This is the first use of malloc() and must come after the
     * static memory layout is mmapped to avoid conflicts with possible
     * use of mmap() by malloc().
     */
    define_var("nil", NIL, TRUE);
    define_var("t", T, TRUE);

    /*
     * Basic algorithm for setting CMUCLLIB and CMUCLCORE, from Pierre
     * Mai.
     *
     * if CMUCLLIB envvar is not set
     *   CMUCLLIB = our list of places to look
     *   if -core option/CMUCLCORE given
     *      CMUCLLIB = CMUCLLIB + full path to the specified core file
     *   endif
     * endif
     *
     * if -core option/CMUCLCORE unset
     *   search for a core file (named whatever arch_init returns or
     *     lisp.core) somewhere in the CMUCLLIB list.
     * endif
     *
     * if core found
     *   use that
     * else
     *   give error message and die
     * endif
     *
     * CMUCLCORE = where the core file was found/specced
     */

    /*
     * Set cmucllib to the -lib option, or to CMUCLLIB envvar.  If
     * neither are set, set cmucllib to our default search path.
     */
    if (lib != NULL) {
	cmucllib = strdup(lib);
    } else {
	char *libvar;

	libvar = getenv("CMUCLLIB");
	if (libvar != NULL) {
	    cmucllib = strdup(libvar);
	} else {
	    char *newlib = NULL;

	    /*
	     * We need to use our default search path.  If a core file
	     * is given, we prepend the directory of the core file to
	     * the search path.
	     */
	    cmucllib = default_cmucllib(argv[0]);
	    if (core != NULL) {
		newlib = prepend_core_path(cmucllib, core);
	    } else if (getenv("CMUCLCORE") != NULL) {
		core = getenv("CMUCLCORE");
		newlib = prepend_core_path(cmucllib, core);
	    }

	    if (newlib != NULL) {
		free(cmucllib);
		cmucllib = newlib;
	    }
	}
    }


    /* Only look for a core file if we're not using a built-in image. */
    if (builtin_image_flag == 0) {
	/*
	 * If no core file specified, search for it in CMUCLLIB
	 */
	if (core == NULL) {
	    if (getenv("CMUCLCORE") == NULL) {
		core = search_core(cmucllib, default_core);
	    } else {
		core = getenv("CMUCLCORE");
	    }
	}

	/* Die if the core file doesn't exist. */
	{
	    struct stat statbuf;

	    if (stat(core, &statbuf) != 0) {
		/* Can't find it so print a message and exit */
		fprintf(stderr, "Cannot find core file");
		if (core != NULL) {
		    fprintf(stderr, " %s", core);
		}
		fprintf(stderr, "\n");
		fprintf(stderr, "Based on lisp binary path `%s'\n", argv[0]);
		exit(1);
	    }
	}
    } else {
	/* The "core file" is the executable.  We have to do this
	 * because this file actually gets checked for later."
	 */
	core = argv[0];
    }

    globals_init();

    if (builtin_image_flag != 0) {
	extern int image_dynamic_space_size;
	long allocation_pointer =

	    (long) dynamic_0_space + (int) &image_dynamic_space_size;
#if defined(i386) || defined(__x86_64)
	SetSymbolValue(ALLOCATION_POINTER, (lispobj) allocation_pointer);
#else
	current_dynamic_space_free_pointer = (lispobj *) allocation_pointer;
#endif
    } else {
	initial_function = load_core_file(core);
    }

#if defined LINKAGE_TABLE
    os_foreign_linkage_init();
#endif /* LINKAGE_TABLE */

#if defined GENCGC
    gencgc_pickup_dynamic();
#else
#if defined WANT_CGC && defined X86_CGC_ACTIVE_P
    {
	extern int use_cgc_p;
	lispobj x = SymbolValue(X86_CGC_ACTIVE_P);

	if (x != type_UnboundMarker && x != NIL)
	    use_cgc_p = 1;	/* enable allocator */
    }
#endif
#endif

#ifdef BINDING_STACK_POINTER
    SetSymbolValue(BINDING_STACK_POINTER, (lispobj) binding_stack);
#endif
#if defined INTERNAL_GC_TRIGGER && !defined i386
    SetSymbolValue(INTERNAL_GC_TRIGGER, make_fixnum(-1));
#endif

    interrupt_init();

    arch_install_interrupt_handlers();
    os_install_interrupt_handlers();

#ifdef PSEUDO_ATOMIC_ATOMIC
    /* Turn on pseudo atomic for when we call into lisp. */
    SetSymbolValue(PSEUDO_ATOMIC_ATOMIC, make_fixnum(1));
    SetSymbolValue(PSEUDO_ATOMIC_INTERRUPTED, make_fixnum(0));
#endif

    /* Convert the argv and envp to something Lisp can grok. */
    SetSymbolValue(LISP_COMMAND_LINE_LIST, alloc_str_list(argv));
    SetSymbolValue(LISP_ENVIRONMENT_LIST, alloc_str_list(envp));

    /* Set cmucllib and cmuclcore appropriately */
    SetSymbolValue(CMUCL_LIB, alloc_string(cmucllib));
    SetSymbolValue(CMUCL_CORE_PATH, alloc_string(core));

    /*
     * Parse the command line again, picking up values that override
     * those loaded from the core.
     */

    argptr = argv;
    while ((arg = *++argptr) != NULL) {
	if (strcmp(arg, "-batch") == 0)
	    SetSymbolValue(BATCH_MODE, T);
    }

    /*
     * Pick off sigint until the lisp system gets far enough along to
     * install it's own.
     */
    sigint_init();

    if (monitor) {
	while (1) {
	    ldb_monitor();
	}
    } else {
	funcall0(initial_function);
	printf("Initial function returned?\n");
	exit(1);
    }
    return 0;			/* not reached */
}
