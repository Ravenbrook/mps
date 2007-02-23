/*
 * $Header: /project/cmucl/cvsroot/src/tools/hpux-startup.c,v 1.4 1995/02/17 12:07:49 wlott Exp $
 *
 * This file copies the lisp startup code, forks a process that will delete
 * the copy when the parent exits, and then exec's the copy of the lisp
 * startup code with the same arguments.  This is needed because of a bug in 
 * hpux.
 */ 

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/file.h>

typedef int boolean;
#define TRUE 1
#define FALSE 0

static char *find_lisp_orig(void)
{
    char *library = getenv("CMUCLLIB");
    char *src, *dst;
    static char buffer[1024];
    boolean found;
    struct stat statbuf;

    if (library == NULL)
	library = "/usr/local/lib/cmucl/lib";

    src = library;
    found = FALSE;
    do {
	dst = buffer;
	while (*src != '\0' && *src != ':')
	    *dst++ = *src++;
	strcpy(dst, "/cmucl.orig");
	if (stat(buffer, &statbuf) != -1)
	    if ((statbuf.st_mode & S_IFMT) == S_IFREG) {
		found = TRUE;
		break;
	    }
    } while (*src++ != '\0');

    if (!found) {
	fprintf(stderr, "Could not find cmucl.orig.  Try setting CMUCLLIB.\n");
	exit(1);
    }    

    return buffer;
}

static char *make_copy(char *source)
{
    static char name[1024];
    int infd, outfd;
    int incount, outcount;
    char buffer[8*1024];
    char *ptr;

    infd = open(source, O_RDONLY, 0);
    if (infd < 0) {
	perror(source);
	exit(1);
    }

    sprintf(name, "/tmp/lisp.copy.XXXXXX");
    outfd = mkstemp(name);
    if (outfd < 0) {
	sprintf(buffer, "open(%s)", name);
	perror(buffer);
	exit(1);
    }

    while ((incount = read(infd, buffer, sizeof(buffer))) > 0) {
	ptr = buffer;
	while ((outcount = write(outfd, ptr, incount)) != incount) {
	    if (outcount < 0) {
		sprintf(buffer, "write(%s)", name);
		perror(buffer);
		exit(1);
	    }
	    incount -= outcount;
	    ptr += outcount;
	}
    }

    if (incount < 0) {
	sprintf(buffer, "read(%s)", source);
	perror(buffer);
	exit(1);
    }

    if (close(infd) < 0) {
	sprintf(buffer, "close(%s)", source);
	perror(buffer);
	exit(1);
    }

    if (close(outfd) < 0) {
	sprintf(buffer, "close(%s)", name);
	perror(buffer);
	exit(1);
    }

    if (chmod(name, 0700) < 0) {
	sprintf(buffer, "chmod(%s)", name);
	perror(buffer);
	exit(1);
    }

    return name;
}

static void spawn_nuker(char *file)
{
    int pid = fork();

    if (pid < 0) {
	perror("fork");
	exit(1);
    }

    if (pid > 0)
	/* The parent process. */
	return;

    if (setpgrp(0, 0) < 0) {
	perror("setpgrp(0,0)");
	exit(1);
    }

    while (getppid() != 1)
	sleep(5);

    unlink(file);

    exit(0);
}

main(int argc, char *argv[])
{
    char *file;

    file = make_copy(find_lisp_orig());

    spawn_nuker(file);

    if (execv(file, argv) < 0) {
	char buffer[256];
	sprintf(buffer, "execv(%s)", file);
	perror(buffer);
	exit(1);
    }
}
