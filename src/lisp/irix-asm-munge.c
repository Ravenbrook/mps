/* This program takes an object file assumed to be the output of assembling
   mips-assem.s and munges certain locations to have the right values, in
   order to work around a bug (SGI may disagree) in SGI's assembler that
   disallows .word, .byte, etc in .text sections in PIC code (but not in
   non_shared).

   Specifically, we stuck "break 11" where we need to munge afterwards, and
   put global labels beforehand. "break 11" is 0x000B000D

   mipsmungelra points to one break which should have been
   .word type_ReturnPcHeader, so we replace it with that value as an int
   undefined_tramp points 4 bytes before two breaks which should be replaced
   by 0x0417FECC and 0x01000000 respectively (they were magic numbers in the
   original code too)
   function_end_breakpoint_guts points to a break that should have been
   .word type_ReturnPcHeader, so replace it too.

   The file is written in place.
   */
#include <stdio.h>
#include <unistd.h>
#include <libelf.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <assert.h>
#include <nlist.h>
#include "lisp.h"
#include "internals.h"

struct nlist nl[] = {
    {"mipsmungelra", 0, 0},
    {"undefined_tramp", 0, 0},
    {"function_end_breakpoint_guts", 0, 0},
    {NULL, 0, 0}
};

main(int argc, char **argv)
{
    int fd;
    Elf *elf;
    Elf32_Ehdr *ehdr;
    Elf_Scn *scn;
    Elf_Data *data;
    Elf32_Half ndx;
    Elf32_Shdr *shdr;
    unsigned int *textdata;

    if (argc != 2)
	exit(2);

    assert(!nlist(argv[1], nl));
    assert(nl[0].n_value && !(nl[0].n_value & 0x3));
    assert(nl[1].n_value && !(nl[1].n_value & 0x3));
    assert(nl[2].n_value && !(nl[2].n_value & 0x3));

    if (elf_version(EV_CURRENT) == EV_NONE) {
	fprintf(stderr, "ELF punted! Recompile %s\n", argv[0]);
	exit(1);
    }

    fd = open(argv[1], O_RDWR);
    if (fd < 0) {
	perror("open");
	exit(1);
    }

    elf = elf_begin(fd, ELF_C_RDWR, NULL);

    ehdr = elf32_getehdr(elf);
    assert(ehdr);
    /* want SHT_PROGBITS section named .text */
    ndx = ehdr->e_shstrndx;
    scn = NULL;
    while ((scn = elf_nextscn(elf, scn))) {
	char *name = NULL;

	if ((shdr = elf32_getshdr(scn)))
	    name = elf_strptr(elf, ndx, (size_t) shdr->sh_name);
	if (!strcmp(name, ".text"))
	    break;
    }

    data = elf_getdata(scn, NULL);
    assert(data && data->d_size && !data->d_off);
    textdata = (unsigned int *) data->d_buf;

    /* process */
    assert(textdata[nl[0].n_value / 4] == 0x000B000D);
    textdata[nl[0].n_value / 4] = type_ReturnPcHeader;

    assert(textdata[nl[1].n_value / 4] == 0x000A000D);
    assert(textdata[nl[1].n_value / 4 + 1] == 0x000B000D);
    assert(textdata[nl[1].n_value / 4 + 2] == 0x000B000D);
    textdata[nl[1].n_value / 4 + 1] = 0x0417FECC;
    textdata[nl[1].n_value / 4 + 2] = 0x01000000;

    assert(textdata[nl[2].n_value / 4] == 0x000B000D);
    textdata[nl[2].n_value / 4] = type_ReturnPcHeader;

    elf_flagdata(data, ELF_C_SET, ELF_F_DIRTY);
    elf_update(elf, ELF_C_WRITE);
    elf_end(elf);
}
