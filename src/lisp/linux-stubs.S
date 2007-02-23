/*	linux-stubs.S

	$Header: /project/cmucl/cvsroot/src/lisp/linux-stubs.S,v 1.20 2005/08/17 03:40:16 rtoy Exp $

	These are needed because the locations of the
	libraries are filled in by ld.so at runtime.
*/

#if defined DARWIN

#define doe(fct) \
.text						@\
.globl  ldso_stub___/**/fct			@\
ldso_stub___/**/fct:				@\
	b ldso_stub__/**/fct/**/stub		@\
.symbol_stub ldso_stub__/**/fct/**/stub:	@\
.indirect_symbol _/**/fct			@\
	lis	r11,ha16(ldso_stub__/**/fct/**/$lazy_ptr)	@\
	lwz	r12,lo16(ldso_stub__/**/fct/**/$lazy_ptr)(r11)	@\
	mtctr	r12				@\
	addi	r11,r11,lo16(ldso_stub__/**/fct/**/$lazy_ptr)	@\
	bctr					@\
.lazy_symbol_pointer				@\
ldso_stub__/**/fct/**/$lazy_ptr:		@\
	.indirect_symbol _/**/fct		@\
	.long dyld_stub_binding_helper

#else

	.file	"linux-stubs.S"
	.version	"01.01"
gcc2_compiled.:
        .text

#define doe(fct) \
	.balign 4 ;\
.globl PVE_stub_ ## fct ;\
	.type	 PVE_stub_ ## fct,@function ;\
PVE_stub_ ## fct: ;\
	jmp fct ;\
.L ## fct ## e1: ;\
	.size	 PVE_stub_ ## fct,.L ## fct ## e1-PVE_stub_ ## fct  ;

#endif

#if defined(LINKAGE_TABLE) && !defined(__FreeBSD__) && !defined(DARWIN)
 /* This is a workaround for glibc braindamage.  These symbols are
    magically transformed by the preprocessor and linker so that
    these symbols of don't appear in libc.so.  These stubs
    are here so that os_dlsym will still work with them.
    Eventually we'll autogenerate this list of symbols by looking
    of the output of 'nm /usr/lib/libc_nonshared.a'. */
 doe(atexit)
 doe(stat)
 doe(fstat)
 doe(lstat)
 doe(stat64)
 doe(fstat64)
 doe(lstat64)
 doe(mknod)
 doe(readdir64)
#else
 doe(accept)
 doe(access)
 doe(acos)
 doe(acosh)
 doe(asin)
 doe(asinh)
 doe(atanh)
 doe(bind)
 doe(cfgetispeed)
 doe(cfgetospeed)
 doe(cfsetispeed)
 doe(cfsetospeed)
 doe(chdir)
 doe(chmod)
 doe(chown)
 doe(close)
 doe(closedir)
 doe(connect)
 doe(cosh)
 doe(creat)
#ifndef DARWIN
 doe(dlclose)
 doe(dlerror)
 doe(dlopen)
 doe(dlsym)
#endif
 doe(dup)
 doe(dup2)
 doe(execve)
 doe(exit)
 doe(fchmod)
 doe(fchown)
 doe(fcntl)
 doe(fork)
 doe(free)
 doe(fstat)
 doe(fsync)
 doe(ftruncate)
 doe(getdtablesize)
 doe(getegid)
 doe(getgid)
 doe(gethostid)
 doe(gethostbyaddr)
 doe(gethostbyname)
 doe(gethostname)
 doe(getitimer)
 doe(getpagesize)
 doe(getpeername)
 doe(getpgrp)
 doe(getpid)
 doe(getppid)
 doe(getrlimit)
 doe(getrusage)
 doe(getsockname)
 doe(getsockopt)
 doe(gettimeofday)
 doe(getuid)
 doe(getwd)
 doe(getcwd)
 doe(hypot)
 doe(ioctl)
 doe(isatty)
 doe(kill)
 doe(killpg)
 doe(link)
 doe(listen)
 doe(log1p)
 doe(lseek)
 doe(lstat)
 doe(malloc)
#ifndef DARWIN
 doe(__malloc_hook)
 doe(__realloc_hook)
 doe(__free_hook)
#endif
 doe(mkdir)
 doe(open)
 doe(opendir)
 doe(pipe)
 doe(pow)
 doe(read)
 doe(readdir)
 doe(readlink)
 doe(recv)
 doe(rename)
 doe(rmdir)
 doe(sched_yield)
 doe(select)
 doe(send)
 doe(setitimer)
 doe(setpgid)
 doe(setpgrp)
 doe(setregid)
 doe(setreuid)
 doe(setrlimit)
 doe(setsockopt)
 doe(sigblock)
 doe(sigpause)
 doe(sigsetmask)
 doe(sinh)
 doe(socket)
 doe(stat)
 doe(symlink)
 doe(sync)
 doe(tanh)
 doe(tcdrain)
 doe(tcflow)
 doe(tcflush)
 doe(tcgetattr)
 doe(tcsendbreak)
 doe(tcsetattr)
 doe(truncate)
 doe(ttyname)
 doe(tzname)
 doe(uname)
 doe(unlink)
 doe(utimes)
 doe(wait3)	
 doe(write)

#if defined(alpha) || defined(DARWIN)
 doe(atan)
 doe(atan2)
 doe(cos)
 doe(exp)
 doe(log)
 doe(log10)
 doe(sin)
 doe(sqrt)
 doe(tan)
#endif


 doe(getpwnam_r)
 doe(getpwuid_r)
 doe(getgrnam_r)
 doe(getgrgid_r)

#if defined(DARWIN)
 doe(getpwuid)
 doe(getpwnam)
#endif

#ifndef DARWIN
 doe(open64)
 doe(creat64)
 doe(lseek64)
 doe(truncate64)
 doe(ftruncate64)
 doe(stat64)
 doe(fstat64)
 doe(lstat64)
 doe(readdir64)
 doe(statfs64)
#endif

/*  doe(abort) */
/*  doe(abs) */
/*  doe(accept) */
/*  doe(access) */
/*  doe(acct) */
/*  doe(acos) */
/*  doe(acosh) */
/*  doe(acoshl) */
/*  doe(acosl) */
/*  doe(addmntent) */
/*  doe(adjtime) */
/*  doe(adjtimex) */
/*  doe(alarm) */
/*  doe(alphasort) */
/*  doe(altzone) */
/*  doe(asctime) */
/*  doe(asin) */
/*  doe(asinh) */
/*  doe(asinhl) */
/*  doe(asinl) */
/*  doe(atan) */
/*  doe(atan2) */
/*  doe(atan2l) */
/*  doe(atanh) */
/*  doe(atanhl) */
/*  doe(atanl) */
/*  doe(atexit) */
/*  doe(atof) */
/*  doe(atoi) */
/*  doe(atol) */
/*  doe(authdes_create) */
/*  doe(authnone_create) */
/*  doe(authunix_create) */
/*  doe(authunix_create_default) */
/*  doe(basename) */
/*  doe(bcmp) */
/*  doe(bcopy) */
/*  doe(bind) */
/*  doe(bindresvport) */
/*  doe(brk) */
/*  doe(bsearch) */
/*  doe(bzero) */
/*  doe(calloc) */
/*  doe(callrpc) */
/*  doe(catclose) */
/*  doe(catgets) */
/*  doe(catopen) */
/*  doe(cbc_crypt) */
/*  doe(cbrt) */
/*  doe(cbrtl) */
/*  doe(ceil) */
/*  doe(ceill) */
/*  doe(cfgetispeed) */
/*  doe(cfgetospeed) */
/*  doe(cfmakeraw) */
/*  doe(cfree) */
/*  doe(cfsetispeed) */
/*  doe(cfsetospeed) */
/*  doe(chdir) */
/*  doe(chmod) */
/*  doe(chown) */
/*  doe(chroot) */
/*  doe(clearerr) */
/*  doe(clnt_broadcast) */
/*  doe(clnt_create) */
/*  doe(clnt_pcreateerror) */
/*  doe(clnt_perrno) */
/*  doe(clnt_perror) */
/*  doe(clnt_spcreateerror) */
/*  doe(clnt_sperrno) */
/*  doe(clnt_sperror) */
/*  doe(clntraw_create) */
/*  doe(clnttcp_create) */
/*  doe(clntudp_bufcreate) */
/*  doe(clntudp_create) */
/*  doe(clock) */
/*  doe(close) */
/*  doe(closedir) */
/*  doe(closelog) */
/*  doe(confstr) */
/*  doe(connect) */
/*  doe(copysign) */
/*  doe(copysignl) */
/*  doe(cos) */
/*  doe(cosh) */
/*  doe(coshl) */
/*  doe(cosl) */
/*  doe(creat) */
/*  doe(crypt) */
/*  doe(ctermid) */
/*  doe(ctime) */
/*  doe(ctime_r) */
/*  doe(cuserid) */
/*  doe(daylight) */
/*  doe(des_setparity) */
/*  doe(difftime) */
/*  doe(dirfd) */
/*  doe(div) */
/*  doe(dlclose) */
/*  doe(dlerror) */
/*  doe(dlopen) */
/*  doe(dlsym) */
/*  doe(dn_comp) */
/*  doe(dn_expand) */
/*  doe(dn_skipname) */
/*  doe(drand48) */
/*  doe(drem) */
/*  doe(dup) */
/*  doe(dup2) */
/*  doe(ecb_crypt) */
/*  doe(ecvt) */
/*  doe(encrypt) */
/*  doe(endgrent) */
/*  doe(endhostent) */
/*  doe(endmntent) */
/*  doe(endnetent) */
/*  doe(endprotoent) */
/*  doe(endpwent) */
/*  doe(endrpcent) */
/*  doe(endservent) */
/*  doe(endsgent) */
/*  doe(endspent) */
/*  doe(endusershell) */
/*  doe(endutent) */
/*  doe(environ) */
/*  doe(erand48) */
/*  doe(erf) */
/*  doe(erfc) */
/*  doe(erfcl) */
/*  doe(erfl) */
/*  doe(execl) */
/*  doe(execle) */
/*  doe(execlp) */
/*  doe(execv) */
/*  doe(execve) */
/*  doe(execvp) */
/*  doe(exit) */
/*  doe(exp) */
/*  doe(expl) */
/*  doe(expm1) */
/*  doe(expm1l) */
/*  doe(fabs) */
/*  doe(fabsl) */
/*  doe(fchdir) */
/*  doe(fchmod) */
/*  doe(fchown) */
/*  doe(fclose) */
/*  doe(fcntl) */
/*  doe(fcvt) */
/*  doe(fdopen) */
/*  doe(feof) */
/*  doe(ferror) */
/*  doe(fflush) */
/*  doe(ffs) */
/*  doe(fgetc) */
/*  doe(fgetgrent) */
/*  doe(fgetpos) */
/*  doe(fgetpwent) */
/*  doe(fgets) */
/*  doe(fgetsgent) */
/*  doe(fgetspent) */
/*  doe(fileno) */
/*  doe(finite) */
/*  doe(flock) */
/*  doe(flockfile) */
/*  doe(floor) */
/*  doe(floorl) */
/*  doe(fmod) */
/*  doe(fmodl) */
/*  doe(fnmatch) */
/*  doe(fopen) */
/*  doe(fork) */
/*  doe(fp_nquery) */
/*  doe(fp_query) */
/*  doe(fp_resstat) */
/*  doe(fpathconf) */
/*  doe(fpclassifyd) */
/*  doe(fpclassifyf) */
/*  doe(fpclassifyl) */
/*  doe(fprintf) */
/*  doe(fputc) */
/*  doe(fputs) */
/*  doe(fread) */
/*  doe(free) */
/*  doe(freopen) */
/*  doe(frexp) */
/*  doe(frexpl) */
/*  doe(fscanf) */
/*  doe(fseek) */
/*  doe(fsetpos) */
/*  doe(fstat) */
/*  doe(fstatfs) */
/*  doe(fsync) */
/*  doe(ftell) */
/*  doe(ftime) */
/*  doe(ftok) */
/*  doe(ftruncate) */
/*  doe(ftrylockfile) */
/*  doe(ftw) */
/*  doe(funlockfile) */
/*  doe(fwrite) */
/*  doe(gcvt) */
/*  doe(get_current_dir_name) */
/*  doe(get_myaddress) */
/*  doe(getc) */
/*  doe(getchar) */
/*  doe(getcwd) */
/*  doe(getdelim) */
/*  doe(getdents) */
/*  doe(getdirentries) */
/*  doe(getdomainname) */
/*  doe(getdtablesize) */
/*  doe(getegid) */
/*  doe(getenv) */
/*  doe(geteuid) */
/*  doe(getgid) */
/*  doe(getgrent) */
/*  doe(getgrgid) */
/*  doe(getgrnam) */
/*  doe(getgroups) */
/*  doe(gethostbyaddr) */
/*  doe(gethostbyname) */
/*  doe(gethostent) */
/*  doe(gethostid) */
/*  doe(gethostname) */
/*  doe(getitimer) */
/*  doe(getlogin) */
/*  doe(getmntent) */
/*  doe(getnetbyaddr) */
/*  doe(getnetbyname) */
/*  doe(getnetent) */
/*  doe(getopt) */
/*  doe(getopt_long) */
/*  doe(getopt_long_only) */
/*  doe(getpagesize) */
/*  doe(getpass) */
/*  doe(getpeername) */
/*  doe(getpgid) */
/*  doe(getpgrp) */
/*  doe(getpid) */
/*  doe(getppid) */
/*  doe(getpriority) */
/*  doe(getprotobyname) */
/*  doe(getprotobynumber) */
/*  doe(getprotoent) */
/*  doe(getpw) */
/*  doe(getpwent) */
/*  doe(getpwnam) */
/*  doe(getpwuid) */
/*  doe(getrpcbyname) */
/*  doe(getrpcbynumber) */
/*  doe(getrpcent) */
/*  doe(getrpcport) */
/*  doe(getrusage) */
/*  doe(gets) */
/*  doe(getservbyname) */
/*  doe(getservbyport) */
/*  doe(getservent) */
/*  doe(getsgent) */
/*  doe(getsgnam) */
/*  doe(getsid) */
/*  doe(getspent) */
/*  doe(getspnam) */
/*  doe(gettimeofday) */
/*  doe(getuid) */
/*  doe(getusershell) */
/*  doe(getutent) */
/*  doe(getutid) */
/*  doe(getutline) */
/*  doe(getw) */
/*  doe(getwd) */
/*  doe(glob) */
/*  doe(globfree) */
/*  doe(gmtime) */
/*  doe(gmtime_r) */
/*  doe(gsignal) */
/*  doe(hasmntopt) */
/*  doe(hcreate) */
/*  doe(hdestroy) */
/*  doe(herror) */
/*  doe(hostalias) */
/*  doe(hsearch) */
/*  doe(htonl) */
/*  doe(htons) */
/*  doe(hypot) */
/*  doe(hypotl) */
/*  doe(idle) */
/*  doe(index) */
/*  doe(inet_addr) */
/*  doe(inet_aton) */
/*  doe(inet_lnaof) */
/*  doe(inet_makeaddr) */
/*  doe(inet_netof) */
/*  doe(inet_network) */
/*  doe(inet_nsap_addr) */
/*  doe(inet_nsap_ntoa) */
/*  doe(inet_ntoa) */
/*  doe(infnan) */
/*  doe(init_des) */
/*  doe(initgroups) */
/*  doe(initstate) */
/*  doe(insque) */
/*  doe(ioctl) */
/*  doe(ioperm) */
/*  doe(iopl) */
/*  doe(ipc) */
/*  doe(isalnum) */
/*  doe(isalpha) */
/*  doe(isascii) */
/*  doe(isatty) */
/*  doe(isblank) */
/*  doe(iscntrl) */
/*  doe(isdigit) */
/*  doe(isgraph) */
/*  doe(isinf) */
/*  doe(isinfl) */
/*  doe(islower) */
/*  doe(isnan) */
/*  doe(isnanl) */
/*  doe(isprint) */
/*  doe(ispunct) */
/*  doe(isspace) */
/*  doe(isupper) */
/*  doe(isxdigit) */
/*  doe(j0) */
/*  doe(j0l) */
/*  doe(j1) */
/*  doe(j1l) */
/*  doe(jn) */
/*  doe(jnl) */
/*  doe(jrand48) */
/*  doe(kill) */
/*  doe(killpg) */
/*  doe(labs) */
/*  doe(lckpwdf) */
/*  doe(lcong48) */
/*  doe(ldexp) */
/*  doe(ldexpl) */
/*  doe(ldiv) */
/*  doe(lfind) */
/*  doe(lgamma) */
/*  doe(lgammal) */
/*  doe(libc_nls_init) */
/*  doe(link) */
/*  doe(listen) */
/*  doe(llseek) */
/*  doe(localeconv) */
/*  doe(localtime) */
/*  doe(localtime_r) */
/*  doe(lockf) */
/*  doe(log) */
/*  doe(log10) */
/*  doe(log10l) */
/*  doe(log1p) */
/*  doe(log1pl) */
/*  doe(log2l) */
/*  doe(logl) */
/*  doe(longjmp) */
/*  doe(lrand48) */
/*  doe(lsearch) */
/*  doe(lseek) */
/*  doe(lstat) */
/*  doe(mallinfo) */
/*  doe(malloc) */
/*  doe(malloc_stats) */
/*  doe(malloc_trim) */
/*  doe(malloc_usable_size) */
/*  doe(mallopt) */
/*  doe(mblen) */
/*  doe(mbstowcs) */
/*  doe(mbtowc) */
/*  doe(memalign) */
/*  doe(memccpy) */
/*  doe(memchr) */
/*  doe(memcmp) */
/*  doe(memcpy) */
/*  doe(memfrob) */
/*  doe(memmem) */
/*  doe(memmove) */
/*  doe(memset) */
/*  doe(mkdir) */
/*  doe(mkfifo) */
/*  doe(mknod) */
/*  doe(mkstemp) */
/*  doe(mktemp) */
/*  doe(mktime) */
/*  doe(mlock) */
/*  doe(mlockall) */
  doe(mmap) 
/*  doe(modf) */
/*  doe(modfl) */
/*  doe(mount) */
/*  doe(mprotect) */
/*  doe(mrand48) */
/*  doe(mremap) */
/*  doe(msgctl) */
/*  doe(msgget) */
/*  doe(msgrcv) */
/*  doe(msgsnd) */
  doe(msync) 
/*  doe(munlock) */
/*  doe(munlockall) */
  doe(munmap) 
/*  doe(nice) */
/*  doe(nl_langinfo) */
/*  doe(nrand48) */
/*  doe(ntohl) */
/*  doe(ntohs) */
/*  doe(obstack_free) */
/*  doe(on_exit) */
/*  doe(open) */
/*  doe(opendir) */
/*  doe(openlog) */
/*  doe(optarg) */
/*  doe(opterr) */
/*  doe(optind) */
/*  doe(optopt) */
/*  doe(p_cdname) */
/*  doe(p_cdnname) */
/*  doe(p_class) */
/*  doe(p_fqname) */
/*  doe(p_option) */
/*  doe(p_query) */
/*  doe(p_rr) */
/*  doe(p_time) */
/*  doe(p_type) */
/*  doe(parse_printf_format) */
/*  doe(pathconf) */
/*  doe(pause) */
/*  doe(pclose) */
/*  doe(perror) */
/*  doe(pipe) */
/*  doe(pmap_getmaps) */
/*  doe(pmap_getport) */
/*  doe(pmap_rmtcall) */
/*  doe(pmap_set) */
/*  doe(pmap_unset) */
/*  doe(popen) */
/*  doe(pow) */
/*  doe(pow10) */
/*  doe(pow10l) */
/*  doe(pow2) */
/*  doe(pow2l) */
/*  doe(powl) */
/*  doe(prev_fstat) */
/*  doe(prev_lstat) */
/*  doe(prev_mknod) */
/*  doe(prev_stat) */
/*  doe(prev_ustat) */
/*  doe(printf) */
/*  doe(psignal) */
/*  doe(ptrace) */
/*  doe(putc) */
/*  doe(putchar) */
/*  doe(putenv) */
/*  doe(putlong) */
/*  doe(putpwent) */
/*  doe(puts) */
/*  doe(putsgent) */
/*  doe(putshort) */
/*  doe(putspent) */
/*  doe(pututline) */
/*  doe(putw) */
/*  doe(qsort) */
/*  doe(raise) */
/*  doe(rand) */
/*  doe(random) */
/*  doe(rcmd) */
/*  doe(re_comp) */
/*  doe(re_compile_fastmap) */
/*  doe(re_compile_pattern) */
/*  doe(re_error_msg) */
/*  doe(re_exec) */
/*  doe(re_match) */
/*  doe(re_match_2) */
/*  doe(re_search) */
/*  doe(re_search_2) */
/*  doe(re_set_registers) */
/*  doe(re_set_syntax) */
/*  doe(re_syntax_options) */
/*  doe(read) */
/*  doe(readdir) */
/*  doe(readdir_r) */
/*  doe(readlink) */
/*  doe(readv) */
/*  doe(realloc) */
/*  doe(realpath) */
/*  doe(reboot) */
/*  doe(recv) */
  doe(recvfrom)
/*  doe(recvmsg) */
/*  doe(regcomp) */
/*  doe(regerror) */
/*  doe(regexec) */
/*  doe(regfree) */
/*  doe(register_printf_function) */
/*  doe(remove) */
/*  doe(remque) */
/*  doe(rename) */
/*  doe(res_init) */
/*  doe(res_isourserver) */
/*  doe(res_mkquery) */
/*  doe(res_nameinquery) */
/*  doe(res_queriesmatch) */
/*  doe(res_query) */
/*  doe(res_querydomain) */
/*  doe(res_randomid) */
/*  doe(res_search) */
/*  doe(res_send) */
/*  doe(rewind) */
/*  doe(rewinddir) */
/*  doe(rexec) */
/*  doe(rindex) */
/*  doe(rint) */
/*  doe(rmdir) */
/*  doe(rpc_createerr) */
/*  doe(rresvport) */
/*  doe(rtime) */
/*  doe(ruserok) */
/*  doe(rx_cache_bound) */
/*  doe(rx_id_instruction_table) */
/*  doe(rx_id_translation) */
/*  doe(rx_slowmap) */
/*  doe(rx_version_string) */
/*  doe(sbrk) */
/*  doe(scandir) */
/*  doe(scanf) */
/*  doe(seed48) */
/*  doe(seekdir) */
/*  doe(select) */
/*  doe(semctl) */
/*  doe(semget) */
/*  doe(semop) */
/*  doe(send) */
/*  doe(sendmsg) */
  doe(sendto)
/*  doe(set_new_handler) */
/*  doe(setbuf) */
/*  doe(setbuffer) */
/*  doe(setdomainname) */
/*  doe(setegid) */
/*  doe(setenv) */
/*  doe(seteuid) */
/*  doe(setfsgid) */
/*  doe(setfsuid) */
  doe(setgid)
/*  doe(setgrent) */
/*  doe(setgroups) */
/*  doe(sethostent) */
/*  doe(sethostid) */
/*  doe(sethostname) */
/*  doe(setitimer) */
/*  doe(setkey) */
/*  doe(setlinebuf) */
/*  doe(setlocale) */
/*  doe(setlogmask) */
/*  doe(setmntent) */
/*  doe(setnetent) */
/*  doe(setpgid) */
/*  doe(setpgrp) */
/*  doe(setpriority) */
/*  doe(setprotoent) */
/*  doe(setpwent) */
/*  doe(setregid) */
/*  doe(setreuid) */
/*  doe(setrpcent) */
/*  doe(setservent) */
/*  doe(setsgent) */
/*  doe(setsid) */
/*  doe(setspent) */
/*  doe(setstate) */
/*  doe(settimeofday) */
  doe(setuid) 
/*  doe(setusershell) */
/*  doe(setutent) */
/*  doe(setvbuf) */
/*  doe(sgetsgent) */
/*  doe(sgetspent) */
/*  doe(shmat) */
/*  doe(shmctl) */
/*  doe(shmdt) */
/*  doe(shmget) */
  doe(shutdown)
/*  doe(sigaction) */
/*  doe(sigaddset) */
/*  doe(sigblock) */
/*  doe(sigdelset) */
/*  doe(sigemptyset) */
/*  doe(sigfillset) */
/*  doe(siggetmask) */
/*  doe(siginterrupt) */
/*  doe(sigismember) */
/*  doe(siglongjmp) */
/*  doe(signal) */
/*  doe(signgam) */
/*  doe(signgaml) */
/*  doe(sigpause) */
/*  doe(sigpending) */
/*  doe(sigprocmask) */
/*  doe(sigreturn) */
/*  doe(sigsetmask) */
/*  doe(sigsuspend) */
/*  doe(sin) */
/*  doe(sinh) */
/*  doe(sinhl) */
/*  doe(sinl) */
/*  doe(sleep) */
/*  doe(snprintf) */
/*  doe(socket) */
/*  doe(socketcall) */
/*  doe(socketpair) */
/*  doe(sprintf) */
/*  doe(sqrt) */
/*  doe(sqrtl) */
/*  doe(srand) */
/*  doe(srand48) */
/*  doe(srandom) */
/*  doe(sscanf) */
/*  doe(ssignal) */
/*  doe(stat) */
/*  doe(statfs) */
/*  doe(stderr) */
/*  doe(stdin) */
/*  doe(stdout) */
/*  doe(stime) */
/*  doe(stpcpy) */
/*  doe(stpncpy) */
/*  doe(strcasecmp) */
/*  doe(strcat) */
/*  doe(strchr) */
/*  doe(strcmp) */
/*  doe(strcoll) */
/*  doe(strcpy) */
/*  doe(strcspn) */
/*  doe(strdup) */
/*  doe(strerror) */
/*  doe(strfry) */
/*  doe(strftime) */
/*  doe(strlen) */
/*  doe(strncasecmp) */
/*  doe(strncat) */
/*  doe(strncmp) */
/*  doe(strncpy) */
/*  doe(strpbrk) */
/*  doe(strptime) */
/*  doe(strrchr) */
/*  doe(strsep) */
/*  doe(strsignal) */
/*  doe(strspn) */
/*  doe(strstr) */
/*  doe(strtod) */
/*  doe(strtof) */
/*  doe(strtok) */
/*  doe(strtol) */
/*  doe(strtold) */
/*  doe(strtoq) */
/*  doe(strtoul) */
/*  doe(strtouq) */
/*  doe(strxfrm) */
/*  doe(svc_exit) */
/*  doe(svc_fdset) */
/*  doe(svc_getreq) */
/*  doe(svc_getreqset) */
/*  doe(svc_register) */
/*  doe(svc_run) */
/*  doe(svc_sendreply) */
/*  doe(svc_unregister) */
/*  doe(svcerr_auth) */
/*  doe(svcerr_decode) */
/*  doe(svcerr_noproc) */
/*  doe(svcerr_noprog) */
/*  doe(svcerr_progvers) */
/*  doe(svcerr_systemerr) */
/*  doe(svcerr_weakauth) */
/*  doe(svcraw_create) */
/*  doe(svctcp_create) */
/*  doe(svcudp_bufcreate) */
/*  doe(svcudp_create) */
/*  doe(swab) */
/*  doe(swapoff) */
/*  doe(swapon) */
/*  doe(symlink) */
/*  doe(sync) */
/*  doe(sys_errlist) */
/*  doe(sys_nerr) */
/*  doe(sys_siglist) */
/*  doe(syscall) */
/*  doe(syscall_flock) */
/*  doe(syscall_readv) */
/*  doe(syscall_writev) */
/*  doe(sysconf) */
/*  doe(sysinfo) */
/*  doe(syslog) */
/*  doe(system) */
/*  doe(tan) */
/*  doe(tanh) */
/*  doe(tanhl) */
/*  doe(tanl) */
/*  doe(tcdrain) */
/*  doe(tcflow) */
/*  doe(tcflush) */
/*  doe(tcgetattr) */
/*  doe(tcgetpgrp) */
/*  doe(tcsendbreak) */
/*  doe(tcsetattr) */
/*  doe(tcsetpgrp) */
/*  doe(tdelete) */
/*  doe(tell) */
/*  doe(telldir) */
/*  doe(tempnam) */
/*  doe(tfind) */
/*  doe(time) */
/*  doe(timegm) */
/*  doe(times) */
/*  doe(timezone) */
/*  doe(tmpfile) */
/*  doe(tmpnam) */
/*  doe(toascii) */
/*  doe(tolower) */
/*  doe(toupper) */
/*  doe(truncate) */
/*  doe(tsearch) */
/*  doe(ttyname) */
/*  doe(ttyname_r) */
/*  doe(twalk) */
/*  doe(tzname) */
/*  doe(tzset) */
/*  doe(ulckpwdf) */
/*  doe(ulimit) */
/*  doe(umask) */
/*  doe(umount) */
/*  doe(ungetc) */
/*  doe(unlink) */
/*  doe(unsetenv) */
/*  doe(uselib) */
/*  doe(usleep) */
/*  doe(ustat) */
/*  doe(utime) */
/*  doe(utimes) */
/*  doe(utmpname) */
/*  doe(valloc) */
/*  doe(vasprintf) */
/*  doe(vfork) */
/*  doe(vfprintf) */
/*  doe(vfscanf) */
/*  doe(vhangup) */
/*  doe(vm86) */
/*  doe(vprintf) */
/*  doe(vscanf) */
/*  doe(vsnprintf) */
/*  doe(vsprintf) */
/*  doe(vsscanf) */
/*  doe(vsyslog) */
/*  doe(wait) */
/*  doe(wait3) */
/*  doe(wait4) */
/*  doe(waitpid) */
/*  doe(wctomb) */
/*  doe(write) */
/*  doe(writev) */
/*  doe(y0) */
/*  doe(y0l) */
/*  doe(y1) */
/*  doe(y1l) */
/*  doe(yn) */
/*  doe(ynl) */
/*  doe(yp_all) */
/*  doe(yp_bind) */
/*  doe(yp_first) */
/*  doe(yp_get_default_domain) */
/*  doe(yp_maplist) */
/*  doe(yp_master) */
/*  doe(yp_match) */
/*  doe(yp_next) */
/*  doe(yp_order) */
/*  doe(yp_unbind) */
/*  doe(yperr_string) */
/*  doe(ypprot_err) */
#endif /* defined(LINKAGE_TABLE) && !defined(__FreeBSD__) */
