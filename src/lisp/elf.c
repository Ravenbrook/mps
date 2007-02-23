/*

 This code was written by Fred Gilham, and has been placed in the
 public domain.  It is provided "as-is" and without warantee of any
 kind.

*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <sys/mman.h>
#include <elf.h>

#include "os.h"


static char elf_magic_string[] = { ELFMAG0, ELFMAG1, ELFMAG2, ELFMAG3 };

/* Note: FreeBSD has nice macros that let you use Elf_Ehdr
   etc. declarations; unfortunately Linux doesn't have this so you
   have to do Elf32_Ehdr etc.  Bad for moving to 64 bits.... */
#if defined(__linux__) || defined(sparc) || defined(__NetBSD__)
typedef Elf32_Ehdr Elf_Ehdr;
typedef Elf32_Shdr Elf_Shdr;
#endif

static Elf_Ehdr eh;

/* Names of the Lisp image ELF sections.  These names must be the same
   as the corresponding names found in the linker script and the
   process-core utility. */
static char *core_section_names[] = { "CORDYN", "CORSTA", "CORRO" };


static void
eread(int d, void *buf, size_t nbytes, const char *func)
{
    int res = read(d, buf, nbytes);

    if (res == -1) {
	perror(func);
	exit(-1);
    }

    if (res < nbytes) {
	fprintf(stderr, "Short read in %s!\n", func);
	exit(-1);
    }
}

static void
elseek(int d, off_t o, const char *func)
{
    if (lseek(d, o, SEEK_SET) == -1) {
	perror(func);
	exit(-1);
    }
}


/* Read the ELF header from a file descriptor and stuff it into a
   structure.  Make sure it is really an elf header etc. */
static void
read_elf_header(int fd, Elf_Ehdr * ehp)
{
    eread(fd, ehp, sizeof(Elf_Ehdr), "read_elf_header");

    if (strncmp(ehp->e_ident, elf_magic_string, 4)) {
	fprintf(stderr,
		"Bad ELF magic number --- not an elf file.  Exiting in %s.\n",
		"read_elf_header");
	exit(-1);
    }
}


static void
read_section_header_entry(int fd, Elf_Shdr * shp)
{
    eread(fd, shp, eh.e_shentsize, "read_section_header_entry");
}


/*
  Map the built-in lisp core sections. 

  NOTE!  We need to do this without using malloc because the memory
  layout is not set until some time after this is done.
*/
void
map_core_sections(char *exec_name)
{
    int exec_fd;
    Elf_Shdr sh;		/* A section header entry. */
    Elf_Shdr strsecent;
    char nambuf[10];
    int soff;
    int strsecoff;		/* File offset to string table section. */
    int sections_remaining = 3;
    int i, j;

    if (!(exec_fd = open(exec_name, O_RDONLY))) {
	perror("Can't open executable!");
	exit(-1);
    }

    read_elf_header(exec_fd, &eh);

    /* Find the section name string section.  Save its file offset. */
    soff = eh.e_shoff + eh.e_shstrndx * eh.e_shentsize;
    elseek(exec_fd, soff, "map_core_sections");
    read_section_header_entry(exec_fd, &strsecent);
    strsecoff = strsecent.sh_offset;

    for (i = 0; i < eh.e_shnum && sections_remaining > 0; i++) {

	/* Read an entry from the section header table. */
	elseek(exec_fd, eh.e_shoff + i * eh.e_shentsize, "map_core_sections");
	read_section_header_entry(exec_fd, &sh);

	/* Read the name from the string section. */
	elseek(exec_fd, strsecoff + sh.sh_name, "map_core_sections");
	eread(exec_fd, nambuf, 6, "map_core_sections");

	if (sh.sh_type == SHT_PROGBITS) {
	    /* See if this section is one of the lisp core sections. */
	    for (j = 0; j < 3; j++) {
		if (!strncmp(nambuf, core_section_names[j], 6)) {
		    /* Found a core section. Map it! */
		    if (os_map
			(exec_fd, sh.sh_offset, (os_vm_address_t) sh.sh_addr,
			 sh.sh_size) == -1) {
			fprintf(stderr, "Can't map section %s\n",
				core_section_names[j]);
			exit(-1);
		    }
		    sections_remaining--;
		    /* Found a core section, don't check the other core section names. */
		    break;
		}
	    }
	}
    }

    close(exec_fd);

    if (sections_remaining != 0) {
	fprintf(stderr, "Couldn't map all core sections!  Exiting!\n");
	exit(-1);
    }
}
