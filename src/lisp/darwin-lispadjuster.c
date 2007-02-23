/* Modify the final lisp binary to enlarge the generated CMUCLRO segment */
#include <stdlib.h>
#include <string.h>
#include <mach-o/loader.h>
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include "validate.h"

#ifdef DEBUG
#define dprintf printf
#else
#define dprintf(...)
#endif

int
main(int argc, char **argv)
{
    char *filename;
    int fd;
    int i, result;
    off_t curpos;
    struct mach_header theheader;
    struct segment_command thesegment;
    char segname[17];

    if (argc != 2)
	return 1;
    filename = argv[1];

    fd = open(filename, O_RDWR, 0755);
    read(fd, &theheader, sizeof(theheader));

    if ((theheader.magic != MH_MAGIC) || (theheader.filetype != MH_EXECUTE))
	return 2;

    dprintf("Reading %d commands:\n", theheader.ncmds);

    for (i = 0; i < theheader.ncmds; i++) {
	curpos = lseek(fd, 0, SEEK_CUR);
	dprintf("Reading command %d (%d bytes):\n", i,

		sizeof(struct load_command));
	read(fd, &thesegment, sizeof(struct load_command));

	dprintf("Command is %d, length %d\n", thesegment.cmd,
		thesegment.cmdsize);
	if (thesegment.cmd != LC_SEGMENT) {
	    dprintf("Skipping remainder of command...\n");
	    lseek(fd, thesegment.cmdsize - sizeof(struct load_command),

		  SEEK_CUR);
	    continue;
	}

	dprintf("Reading remainder of Segment command (%d bytes)",
		sizeof(thesegment) - sizeof(struct load_command));
	read(fd, &thesegment.segname,

	     sizeof(thesegment) - sizeof(struct load_command));
	memset(segname, 0, 17);
	memcpy(segname, thesegment.segname, 16);
	dprintf("Segname: %s\n", segname);
	if (0 == strncmp(thesegment.segname, "CMUCLRO", 7)) {
	    dprintf("Frobbing the segment, setting vmsize from %8X to %8X\n",
		    thesegment.vmsize, READ_ONLY_SPACE_SIZE);
	    thesegment.vmsize = READ_ONLY_SPACE_SIZE;
	    dprintf("Skipping back to %lld.\n", curpos);
	    lseek(fd, curpos, SEEK_SET);
	    dprintf("Writing %d bytes.\n", sizeof(thesegment));
	    result = write(fd, &thesegment, sizeof(thesegment));
	    if (result == -1)
		perror("Error:");
	    close(fd);
	    return 0;
	} else {
	    dprintf("Cmdsize = %d, Already read = %d, Now seeking %d\n",
		    thesegment.cmdsize, sizeof(thesegment),
		    thesegment.cmdsize - sizeof(thesegment));
	    lseek(fd, thesegment.cmdsize - sizeof(thesegment), SEEK_CUR);
	}
    }
    close(fd);
    return 3;
}
