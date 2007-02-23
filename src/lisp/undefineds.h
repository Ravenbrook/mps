/* Routines that must be linked into the core for lisp to work. */
/* $Header: /project/cmucl/cvsroot/src/lisp/undefineds.h,v 1.37 2005/09/15 18:26:53 rtoy Exp $ */

#ifndef _UNDEFINEDS_H_
#define _UNDEFINEDS_H_

/* Pick up all the syscalls. */
F(accept)
    F(access)
    F(acct)
#ifndef hpux
    F(adjtime)
#endif
    F(bind)
    F(brk)
#if defined(hpux) || defined(SVR4) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__)
    F(cfgetospeed)
    F(cfsetospeed)
    F(cfgetispeed)
    F(cfsetispeed)
#endif
    F(chdir)
    F(chmod)
    F(chown)
    F(chroot)
    F(close)
    F(connect)
    F(creat)
    F(dup)
    F(dup2)
    F(execve)
    F(exit)
    F(fchmod)
    F(fchown)
    F(fcntl)
#if !defined(hpux) && !defined(SVR4)
    F(flock)
#endif
    F(fork)
    F(fstat)
    F(fsync)
    F(ftruncate)
#if !defined(hpux) && !defined(SVR4) || defined(SOLARIS25) || defined(irix)
    F(getdtablesize)
#endif
    F(getegid)
    F(geteuid)
    F(getgid)
    F(getgroups)
#if !defined (SOLARIS) || defined(SOLARIS25)
    F(gethostid)
#endif
    F(gethostname)
    F(getitimer)
#if !defined(hpux) && !defined(SVR4) || defined(SOLARIS25)
    F(getpagesize)
#endif
    F(getpeername)
    F(getpgrp)
    F(getpid)
    F(getppid)
#if !defined(SVR4)  ||  defined(SOLARIS25)
    F(getpriority)
#endif
    F(getrlimit)
#if !defined(SOLARIS) ||  defined(SOLARIS25)
    F(getrusage)
#endif
    F(getsockname)
    F(getsockopt)
    F(gettimeofday)
    F(getuid)
    F(ioctl)
    F(kill)
#if !defined(SOLARIS) || defined(SOLARIS25)
    F(killpg)
#endif
    F(link)
    F(listen)
    F(lseek)
    F(lstat)
    F(mkdir)
    F(mknod)
    F(mmap)
    F(mount)
    F(msync)
    F(munmap)
    F(open)
    F(pipe)
    F(profil)
    F(ptrace)
#ifdef mach
    F(quota)
#endif
    F(read)
    F(readlink)
    F(readv)
#ifndef SVR4
    F(reboot)
#endif
    F(recv)
    F(recvfrom)
    F(recvmsg)
    F(rename)
    F(rmdir)
    F(sbrk)
    F(select)
    F(send)
    F(sendmsg)
    F(sendto)
    F(setgroups)
#if !defined(SUNOS) && !(defined(SOLARIS) ||  defined(SOLARIS25))
    F(sethostid)
#endif
#if !defined(SVR4) ||  defined(SOLARIS25)
    F(sethostname)
#endif
    F(setitimer)
    F(setpgrp)
#if !defined(SVR4) ||  defined(SOLARIS25)
    F(setpriority)
#endif
#if !defined(mach) && !defined(SOLARIS) && !defined(__FreeBSD__) && !defined(__OpenBSD__) && !defined(__NetBSD__) && !defined(SUNOS) && !defined(osf1) && !defined(irix) && !defined(hpux) && !(defined(linux) && defined(alpha))
    F(setquota)
#endif
#if !defined(hpux) && !defined(SVR4) ||  defined(SOLARIS25)
    F(setregid)
    F(setreuid)
#endif
    F(setrlimit)
    F(setsockopt)
    F(settimeofday)
    F(setgid)
    F(setuid)
    F(shutdown)
#ifndef SVR4
    F(sigblock)
#endif
    F(sigpause)
#if !defined(ibmrt) && !defined(hpux) && !defined(SVR4) && !defined(i386)
    F(sigreturn)
#endif
#if !defined(SVR4) && !defined(__FreeBSD__) && !defined(__OpenBSD__) && !defined(__NetBSD__) && !defined(DARWIN)
    F(sigsetmask)
    F(sigstack)
    F(sigvec)
#endif
    F(socket)
    F(socketpair)
    F(stat)
#ifndef SVR4
    F(swapon)
#endif
    F(symlink)
    F(sync)
    F(syscall)
#if defined(hpux) || defined(SVR4)
    F(closedir)
    F(opendir)
    F(readdir)
#endif
#if defined(hpux) || defined(SVR4) || defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__linux__)
    F(tcgetattr)
    F(tcsetattr)
    F(tcsendbreak)
    F(tcdrain)
    F(tcflush)
    F(tcflow)
#endif
#if defined(SOLARIS)
    F(times)
#endif
    F(truncate)
    F(umask)
#if !defined(SUNOS) && !defined(parisc) && !defined(SOLARIS) \
  && !defined(__FreeBSD__) && !defined(__OpenBSD__) && !defined(__NetBSD__) \
  && !defined(DARWIN)
    F(umount)
#endif
    F(unlink)
#ifndef hpux
    F(utimes)
#endif
#ifndef irix
    F(vfork)
#endif
#if !defined(osf1) && !defined(__FreeBSD__) && !defined(__OpenBSD__) && !defined(__NetBSD__) && !defined(DARWIN)
    F(vhangup)
#endif
    F(wait)
#if !defined(SOLARIS) ||  defined(SOLARIS25)
    F(wait3)
#endif
    F(write)
    F(writev)

/* Math routines. */
    F(cos)
    F(sin)
    F(tan)
    F(acos)
    F(asin)
    F(atan)
    F(atan2)
    F(sinh)
    F(cosh)
    F(tanh)
    F(asinh)
    F(acosh)
    F(atanh)
    F(exp)
#ifndef hpux
    F(expm1)
#endif
    F(log)
    F(log10)
#ifndef hpux
    F(log1p)
#endif
    F(pow)
#ifndef hpux
    F(cbrt)
#endif
#ifndef i386
    F(sqrt)
#endif
    F(hypot)

/* Network support. */
    F(gethostbyname)
    F(gethostbyaddr)

/* Other random things. */
#if defined(SVR4)
    F(setpgid)
    F(getpgid)
    D(timezone)
    D(altzone)
    D(daylight)
    D(tzname)
#endif
#if defined(SVR4) || defined(__OpenBSD__)
    F(dlopen)
    F(dlsym)
    F(dlclose)
    F(dlerror)
#endif
#if !defined (SOLARIS) ||  defined(SOLARIS25)
    F(getwd)
    F(getcwd)
#endif
    F(ttyname)
#ifdef irix
    F(_getpty)
#endif
#if ( defined(alpha) && defined(linux) )
    F(dlopen)
    F(dlsym)
    F(dlclose)
    F(dlerror)
    F(cfgetospeed)
    F(cfsetospeed)
    F(cfgetispeed)
    F(cfsetispeed)
    F(opendir)
    F(closedir)
    F(readdir)
    F(sched_yield)
    F(setpgid)
    D(tzname)
    D(errno)
    F(open64)
    F(creat64)
    F(lseek64)
    F(truncate64)
    F(ftruncate64)
    F(stat64)
    F(fstat64)
    F(lstat64)
    F(readdir64)
    F(statfs64)
    F(lockf64)
#endif
#if defined(sparc) || defined(linux)
    F(getpwnam_r)
    F(getpwuid_r)
    F(getgrnam_r)
    F(getgrgid_r)
#endif
#if defined(__NetBSD__) || defined(DARWIN)
    F(getpwnam)
    F(getpwuid)
    F(getgrnam)
    F(getgrgid)
#endif
    F(setpwent)
    F(getpwent)
    F(endpwent)
#endif /* _UNDEFINEDS_H_ */
