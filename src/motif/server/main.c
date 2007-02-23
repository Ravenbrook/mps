/*

 $Header: /project/cmucl/cvsroot/src/motif/server/main.c,v 1.14 2001/03/08 00:08:50 pw Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>
#include <sys/param.h>		/* for define of BSD */
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <errno.h>
#include <signal.h>

#include <sys/wait.h>

#ifdef __linux__
#include <asm/posix_types.h>
#include <linux/posix_types.h>
#include <linux/types.h>
#if !defined(FD_ZERO)
#define FD_ZERO __FD_ZERO
#define FD_SET __FD_SET
#define FD_ISSET __FD_ISSET
#endif
#endif

#ifdef SVR4
#define bzero(a,n) memset(a, 0, n)
#endif

#define PORT 8000

#if    !defined(MAX)
#define MAX(x,y) ((x<y)?y:x)
#endif

/* Some things (ie. RT) don't define this in errno.h.  Go figure.  */
#ifndef SVR4
extern int errno;
#endif

extern void serve_client(int socket);

/* Sockets the server will accept connections on */
int unix_socket, inet_socket;
char socket_path[30];

/* Addresses of the server */
struct sockaddr_in inetAddress;
struct sockaddr_un unixAddress;

int global_argc;
char **global_argv;
int global_will_trace=NULL;
int will_fork=1;
int use_inet_socket=1;
int use_unix_socket=1;

enum { Global, Normal, Local } socket_choice = Normal;

#if defined(BSD) || defined(linux)
void bury_zombie ()
{
  /* This is called to handle SIGCHLD. Wait3 lets BSD lay
   * the dead child process to rest. Otherwise it hangs 
   * around until the server dies. During development this can
   * eat up a lot of swap space.
   */
  int status;
  wait3(&status, WNOHANG, NULL);
}
#endif
void close_sockets()
{
  close(unix_socket);
  close(inet_socket);
  unlink(socket_path);
}

void server_shutdown()
{
  fprintf(stderr,"Caught signal.\n");
  close_sockets();
  exit(0);
}

void main_err(char *message)
{
  fprintf(stderr,"%s\n",message);
  if( errno )
    perror(NULL);

  close_sockets();

  exit(1);
}

void create_unix_socket(char *path)
{
  unix_socket = socket(AF_UNIX, SOCK_STREAM, 0);
  if( unix_socket < 0 )
    main_err("create_unix_socket:  Unable to open Unix domain socket.");

  unixAddress.sun_family = AF_UNIX;
  strcpy(unixAddress.sun_path, path);
  if( bind(unix_socket, (struct sockaddr *)&unixAddress,
	   strlen(path)+2) < 0 ) {
    main_err("create_unix_socket:  Unable to bind Unix socket.");
  }

  if( listen(unix_socket,5) < 0 )
    main_err("create_unix_socket:  Unable to listen on Unix socket.");
}

void create_inet_socket(int port)
{
  inet_socket = socket(AF_INET, SOCK_STREAM, 0);
  if( inet_socket < 0 )
    main_err("create_inet_socket:  Unable to open Inet domain socket.");

  bzero((char *)&inetAddress, sizeof(inetAddress));
  inetAddress.sin_family      = AF_INET;
  inetAddress.sin_port        = htons(port);
  inetAddress.sin_addr.s_addr = htonl(INADDR_ANY);
  if( bind(inet_socket, (struct sockaddr *)&inetAddress,
	   sizeof(inetAddress)) < 0 ) {
    main_err("create_inet_socket:  Unable to bind Inet socket.");
  }

  if( listen(inet_socket,5) < 0 )
    main_err("create_inet_socket:  Unable to listen on Inet socket.");
}


void start_server(int port, char *path)
{
  if( use_unix_socket )
    create_unix_socket(path);
  if( use_inet_socket )
    create_inet_socket(port);
}

void establish_client(int s)
{
  int pid=0,socket;
  struct sockaddr sa;
  int sa_length = sizeof(struct sockaddr);

  socket = accept(s, &sa, &sa_length);
  if( socket < 0 )
    main_err("establish_client:  Unable to accept client connection.");

  printf("Accepted client on fd %d\n",socket);
  fflush(stdout);

  if( will_fork )
    pid = fork();
  else
    printf("Server not forking.\n");

  fflush(stdout);
  switch( pid ) {
  case 0:    /*  The child process */
    close(unix_socket);
    close(inet_socket);
    serve_client(socket);
    break;
  case -1:
    main_err("establish_client:  Fork failed.");
    break;
  default:
    close(socket);
    break;
  }
}

main(int argc, char **argv)
{
  fd_set rfds;
  int nfound,nfds,i;
  int port = PORT;
#ifdef MACH
  union wait status;
#endif

  /* This is so resources can be passed to the servers on the command line */
  global_argc = argc;
  global_argv = argv;

  /*
   * - Invoking the server with "-nofork" will ensure that the server
   *   does fork when handling client connections.
   * - Invoking the server with "-trace" will make the server print
   *   out all sorts of useful trace info while it's running.
   */
  for(i=1;i<argc;i++) {
    if( !strcmp(argv[i],"-trace") )
      global_will_trace = 1;
    else if( !strcmp(argv[i],"-nofork") )
      will_fork = 0;
    else if( !strcmp(argv[i],"-global") )
      socket_choice = Global;
    else if( !strcmp(argv[i],"-local") )
      socket_choice = Local;
    else if( !strcmp(argv[i],"-noinet") )
      use_inet_socket = NULL;
    else if( !strcmp(argv[i],"-nounix") )
      use_unix_socket = NULL;
  }

  switch( socket_choice ) {
  case Global:
    sprintf(socket_path,"/tmp/.motif_socket");
    break;
  case Normal:
    port += getuid();
    sprintf(socket_path,"/tmp/.motif_socket-u%d",getuid());
    break;
  case Local:
    use_inet_socket = NULL;
    sprintf(socket_path,"/tmp/.motif_socket-p%d",getpid());
    break;
  }

  printf("Starting server:\n");
  printf("   will_fork  = %s\n",will_fork?"True":"False");
  printf("   will_trace = %s\n",global_will_trace?"True":"False");
  if( use_inet_socket )
    printf("   port       = %d\n",port);
  else
    printf("   No Inet domain socket created.\n");
  if( use_unix_socket )
    printf("   path       = %s\n",socket_path);
  else
    printf("   No Unix domain socket created.\n");

  fflush(stdout);

  start_server(port, socket_path);

  /* Catch signals and remove our sockets before going away. */
  signal(SIGHUP, server_shutdown);
  signal(SIGINT, server_shutdown);
  signal(SIGQUIT, server_shutdown);
#if defined(BSD) || defined(linux)
  signal(SIGCHLD, bury_zombie);
#elif defined(hpux) || defined(SVR4)
  signal(SIGCHLD, SIG_IGN);
#endif

  printf("Waiting for connection.\n");
  fflush(stdout);

  FD_ZERO(&rfds);
  nfds = MAX(unix_socket,inet_socket)+1;
  for(;;) {
    if( use_unix_socket ) FD_SET(unix_socket, &rfds);
    if( use_inet_socket ) FD_SET(inet_socket, &rfds);
    nfound = select(nfds, &rfds, NULL, NULL, 0);
    if( nfound < 0 && errno != EINTR )
      main_err("main:  Unable to select on sockets.");
    else if( nfound > 0 ){
      if( FD_ISSET(unix_socket, &rfds) ) {
	printf("Accepting client on Unix socket.\n");
	fflush(stdout);
	establish_client(unix_socket);
      }
      if( FD_ISSET(inet_socket, &rfds) ) {
	printf("Accepting client on Inet socket.\n");
	fflush(stdout);
	establish_client(inet_socket);
      }
      /* Prevent zombie children under Mach */
#ifdef MACH
      wait3(&status,WNOHANG,NULL);
#endif
    }
  }
}
