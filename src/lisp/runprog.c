/*
 * $Header: /project/cmucl/cvsroot/src/lisp/runprog.c,v 1.7 2005/09/15 18:26:52 rtoy Exp $
 *
 * Support for run-program.
 *
 */

#include <sys/ioctl.h>
#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdlib.h>
#include <termios.h>
#include <unistd.h>

pid_t
spawn(char *program, char *argv[], char *envp[], char *pty_name,
      int stdin, int stdout, int stderr)
{
    pid_t pid;
    sigset_t set;
    int fd;

    pid = fork();
    if (pid != 0)
	return pid;

    /* Put us in our own process group. */
    setsid();

    /* Unblock all signals. */
    sigemptyset(&set);
    sigprocmask(SIG_SETMASK, &set, NULL);

    /* If we are supposed to be part of some other pty, go for it. */
    if (pty_name) {
#ifdef TIOCNOTTY
	fd = open("/dev/tty", O_RDWR, 0);
	if (fd >= 0) {
	    ioctl(fd, TIOCNOTTY, 0);
	    close(fd);
	}
#endif

	fd = open(pty_name, O_RDWR, 0);
	dup2(fd, 0);
	dup2(fd, 1);
	dup2(fd, 2);
	close(fd);
    }

    /* Set up stdin, stdout, and stderr. */
    if (stdin >= 0)
	dup2(stdin, 0);
    if (stdout >= 0)
	dup2(stdout, 1);
    if (stderr >= 0)
	dup2(stderr, 2);

    /* Close all other fds. */
    for (fd = sysconf(_SC_OPEN_MAX) - 1; fd >= 3; fd--)
	close(fd);

    /* Exec the program. */
    execve(program, argv, envp);

    /* It didn't work, so try /bin/sh. */
    argv[0] = program;
    argv[-1] = "sh";
    execve("/bin/sh", argv - 1, envp);

    /* The exec didn't work, flame out. */
    exit(1);
}
