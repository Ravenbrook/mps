/* $Header: /project/cmucl/cvsroot/src/lisp/coreparse.c,v 1.10 2005/09/15 18:26:51 rtoy Exp $ */
#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>

#if defined(irix) || defined(sparc)
#include <fcntl.h>
#include <stdlib.h>
#endif

#include "os.h"
#include "lisp.h"
#include "globals.h"
#include "core.h"
#include "internals.h"

extern int version;

static void
process_directory(int fd, long *ptr, int count)
{
    long id, offset, len;
    lispobj *free_pointer;
    os_vm_address_t addr;
    struct ndir_entry *entry;

    entry = (struct ndir_entry *) ptr;

    while (count-- > 0) {
	id = entry->identifier;
	offset = CORE_PAGESIZE * (1 + entry->data_page);
	addr = (os_vm_address_t) (CORE_PAGESIZE * entry->address);
	free_pointer = (lispobj *) addr + entry->nwords;
	len = CORE_PAGESIZE * entry->page_count;

	if (len != 0) {
	    os_vm_address_t real_addr;

#ifdef PRINTNOISE
	    printf("Mapping %ld bytes at 0x%lx.\n", len, addr);
#endif
	    real_addr = os_map(fd, offset, addr, len);
	    if (real_addr != addr)
		fprintf(stderr,
			"process_directory: file mapped in wrong place! (0x%p != 0x%p)\n",
			(void *) real_addr, (void *) addr);
	}
#if 0
	printf("Space ID = %d, free pointer = 0x%08x.\n", id, free_pointer);
#endif

	switch (id) {
	  case DYNAMIC_SPACE_ID:
	      if (addr != (os_vm_address_t) dynamic_0_space
		  && addr != (os_vm_address_t) dynamic_1_space)
		  printf("Strange ... dynamic space lossage.\n");
	      current_dynamic_space = (lispobj *) addr;
#if defined(ibmrt) || defined(i386) || defined(__x86_64)
	      SetSymbolValue(ALLOCATION_POINTER, (lispobj) free_pointer);
#else
	      current_dynamic_space_free_pointer = free_pointer;
#endif
	      break;
	  case STATIC_SPACE_ID:
	      static_space = (lispobj *) addr;
	      break;
	  case READ_ONLY_SPACE_ID:
	      /* Don't care about read only space */
	      break;
	  default:
	      printf("Strange space ID: %ld; ignored.\n", id);
	      break;
	}
	entry++;
    }
}

lispobj
load_core_file(char *file)
{
    int fd = open(file, O_RDONLY), count;

#if !(defined(alpha) || defined(__x86_64))
    long header[CORE_PAGESIZE / sizeof(long)], val, len, *ptr;
#else
    u32 header[CORE_PAGESIZE / sizeof(u32)], val, len, *ptr;
#endif
    lispobj initial_function = NIL;

    if (fd < 0) {
	fprintf(stderr, "Could not open file \"%s\".\n", file);
	perror("open");
	exit(1);
    }

    count = read(fd, header, CORE_PAGESIZE);
    if (count < 0) {
	perror("read");
	exit(1);
    }
    if (count < CORE_PAGESIZE) {
	fprintf(stderr, "Premature EOF.\n");
	exit(1);
    }

    ptr = header;
    val = *ptr++;

    if (val != CORE_MAGIC) {
	fprintf(stderr, "Invalid magic number: 0x%lx should have been 0x%x.\n",
		val, CORE_MAGIC);
	exit(1);
    }

    while (val != CORE_END) {
	val = *ptr++;
	len = *ptr++;

	switch (val) {
	  case CORE_END:
	      break;

	  case CORE_VERSION:
	      if (*ptr != version) {
		  fprintf(stderr,
			  "WARNING: startup-code version (%d) different from core version (%ld).\nYou may lose big.\n",
			  version, *ptr);
	      }
	      break;

	  case CORE_VALIDATE:
	      fprintf(stderr, "Validation no longer supported; ignored.\n");
	      break;

	  case CORE_NDIRECTORY:
	      process_directory(fd, ptr,
#if !(defined(alpha) || defined(__x86_64))
				(len - 2) / (sizeof(struct ndir_entry) / sizeof(long)));
#else
				(len - 2) / (sizeof(struct ndir_entry) / sizeof(u32)));
#endif
	      break;

	  case CORE_INITIAL_FUNCTION:
	      initial_function = (lispobj) * ptr;
	      break;

	  case CORE_MACHINE_STATE:
	      fprintf(stderr, "Obsolete core file.\n");
	      exit(1);
	      break;

	  default:
	      printf("Unknown core file entry: %ld; skipping.\n", val);
	      break;
	}

	ptr += len - 2;
    }

    return initial_function;
}
