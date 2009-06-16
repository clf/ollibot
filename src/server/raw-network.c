
/* C glue to interface ML to the UNIX socket library.
   Written by Tom Murphy VII and Ruy Ley-Wild in 2003--
*/

#include <sys/poll.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <string.h>
#include <malloc.h>

int ML_AF_UNIX = AF_UNIX;
int ML_PF_INET = PF_INET;

int ML_SOCK_STREAM = SOCK_STREAM;

#ifndef MSG_NOSIGNAL
int ML_MSG_NOSIGNAL = 0;
#else
int ML_MSG_NOSIGNAL = MSG_NOSIGNAL;
#endif

// #undef MSG_NOSIGNAL

#ifndef MSG_DONTWAIT
int ML_MSG_DONTWAIT = 0;
#else
int ML_MSG_DONTWAIT = MSG_DONTWAIT;
#endif

int ML_EAGAIN = EAGAIN;

int ml_errno = 0;

void getlocalip(char *ipaddr) {
  int i;
  struct hostent *host;
  char hostname[512];

  gethostname(hostname, 511);
  hostname[511] = '\0';
  host = gethostbyname(hostname);
  for (i = 0; i < 4; i++)
    ipaddr[i] = host->h_addr_list[0][i];
}

int ml_close(int fd) {
  /* XXX: correct?
     shutdown alone appears to leak socket descriptors on linux
     (and probably windows, too).

     close appears to block for 4 minutes on windows */
  shutdown(fd, SHUT_RDWR);
  close(fd);
  return 0;
}

int ml_connectux(char * path) {
  struct sockaddr_un saun;
  int maxlen;


  int s = socket(AF_UNIX, SOCK_STREAM, 0);
  if (s == -1)
    ; /*printf("SOCKET ERROR: %s\n", strerror(errno));*/
  ml_errno = errno;

  if (s < 0) return -1;

  /* XXX: shouldn't there be a system constant for this?? */
  maxlen = (sizeof (saun)) - ((int)&saun.sun_path - (int)&saun) - 2;


  if (strlen(path) > maxlen) return -1;

  saun.sun_family = AF_UNIX;
  strcpy(saun.sun_path, path);

  if (connect(s, (struct sockaddr*)&saun, sizeof (saun)) < 0) {
    //printf("CONNECT ERROR: %s\n", strerror(errno));
    ml_errno = errno;
    /*    printf("oops: %s\n", strerror(errno)); */
    return -1;
  }

  return s;

}

int ml_uslisten (char * path) {

  struct sockaddr_un svr;
  int e, maxlen;
  int s = socket(AF_UNIX, SOCK_STREAM, 0);
  if(s == -1)
    /* printf("SOCKET ERROR: %s\n", strerror(errno)); */ ;
  ml_errno = errno;

  if (s < 0) return -1;

  /* XXX: shouldn't there be a system constant for this?? */
  maxlen = (sizeof (svr)) - ((int)&svr.sun_path - (int)&svr) - 2;

  if (strlen(path) > maxlen) return -1;

  svr.sun_family = AF_UNIX;
  strcpy(svr.sun_path, path);
  (void) unlink(path);
  
  if((e = bind(s, (struct sockaddr *)&svr, sizeof (struct sockaddr_un))) == -1)
    printf("BIND ERROR: %s\n", strerror(errno));
  ml_errno = errno;

  if (e < 0) return -1;
  
  if((e = listen (s, 10)) == -1)
    printf("LISTEN ERROR: %s\n", strerror(errno));
  ml_errno = errno;

  if (e < 0) return -1;

  /* Success! */
  return s;
}

int ml_get_errno() {
  return ml_errno;
}

int ml_recv (int s, char * buf, int len) {
  int res;

#ifndef MSG_NOSIGNAL
  sigset_t new, old;
  /* ML_MSG_NOSIGNAL -> block SIGPIPE
   */
  if (sigemptyset(&new))
      return -1;
  if (sigaddset(&new, SIGPIPE))
      return -1;
  if (sigprocmask(SIG_BLOCK, &new, &old))
      return -1;
#endif
  res = recv(s, buf, len, ML_MSG_NOSIGNAL);
#ifndef MSG_NOSIGNAL
  if (sigprocmask(SIG_BLOCK, &old, NULL))
      return -1;
#endif
  ml_errno = errno;
  return res;
}

int ml_send (int s, char * buf, int len) {
  int res;

#ifndef MSG_NOSIGNAL
  sigset_t new, old;
  /* ML_MSG_NOSIGNAL -> block SIGPIPE
   */
  if (sigemptyset(&new))
      return -1;
  if (sigaddset(&new, SIGPIPE))
      return -1;
  if (sigprocmask(SIG_BLOCK, &new, &old))
      return -1;
#endif

#ifndef MSG_DONTWAIT
  /* ML_MSG_DONTWAIT -> fcntl(...O_NONBLOCK)
   */
  fcntl(s, F_SETFL, fcntl(s, F_GETFL) | O_NONBLOCK);
#endif

  res = send(s, buf, len, ML_MSG_NOSIGNAL | ML_MSG_DONTWAIT);

#ifndef MSG_DONTWAIT
  fcntl(s, F_SETFL, fcntl(s, F_GETFL) & ~O_NONBLOCK);
#endif

#ifndef MSG_NOSIGNAL
  if (sigprocmask(SIG_BLOCK, &old, NULL))
      return -1;
#endif
  
  ml_errno = errno;

  return res;
}

int ml_haserror(int sd) {
  int err, errlen = sizeof(err);
  return getsockopt(sd, SOL_SOCKET, SO_ERROR, &err, &errlen);
}

int ml_connect(const char * a, int p) {

  int s;
  struct sockaddr_in dest;
  struct hostent * h;

  if((s = socket(AF_INET, SOCK_STREAM, 0)) == -1)
  {
    printf("SOCKET ERROR: %s\n", strerror(errno));
    return -1;
  }

  if ((h = (struct hostent *)gethostbyname(a))) {
    dest.sin_addr = *((struct in_addr *)h->h_addr);
  } else dest.sin_addr.s_addr = inet_addr(a);

  /*
  printf("Connecting to IP: %d.%d.%d.%d \n",
	 255 & ((int)dest.sin_addr.s_addr >> 24),
	 255 & ((int)dest.sin_addr.s_addr >> 16),
	 255 & ((int)dest.sin_addr.s_addr >> 8),
	 255 & (int)dest.sin_addr.s_addr);
  */
  dest.sin_family = AF_INET;
  dest.sin_port = htons(p);

  memset(&(dest.sin_zero), 0, 8);

  /* set non-blocking for the connect operation. */
  /* XXX I understand I should use F_GETFL to get
     the old flags, first, so that I can preserve them. */
  fcntl(s, F_SETFL, O_NONBLOCK);

  if(connect(s, (struct sockaddr *) &dest, sizeof (struct sockaddr)) == -1)
    /*printf("CONNECT ERROR: %s\n", strerror(errno))*/;

  fcntl(s, F_SETFD, 1); /* don't keep open across exec */

  return s;
}

int ml_bindport(int s, int p) {
  struct sockaddr_in addr;
      
  addr.sin_family = AF_INET;
  addr.sin_port = htons(p);
  addr.sin_addr.s_addr = 0; /* INADDR_ANY */
  memset(&(addr.sin_zero), '\0', 8);

  /* XXX SO_REUSEADDR */

  /* try our best to rebind even if we recently used it... */
  int val = 1;
  /* these have error codes, but we're going to continue either way, so... */
  setsockopt(s, SOL_SOCKET, SO_REUSEADDR, (char *)&val, sizeof(val));
  /* not available? */
  //  setsockopt(s, SOL_SOCKET, SO_REUSEPORT, (char *)&val, sizeof(val));

  return bind(s, (struct sockaddr *)&addr, sizeof(struct sockaddr));
}

int ml_accept(int l) {
  struct sockaddr remote;
  int sin_size = sizeof (struct sockaddr_in);
  int e = 0;
  sigset_t blockme;


  if (sigfillset(&blockme))
    return -1;
  if (sigprocmask(SIG_BLOCK, &blockme, NULL))
    return -1;
  if ((e = accept(l, (struct sockaddr*)&remote, &sin_size)) == -1)
    printf("ACCEPT ERROR: %s\n", strerror(errno));
  if (sigprocmask(SIG_UNBLOCK, &blockme, NULL))
    return -1;

  fcntl(e, F_SETFD, 1); /* don't keep open across exec */

  return e;
}


int ml_peername(int s, char * dest) {
  struct sockaddr_in peer;
  int peer_len;
 
  peer_len = sizeof(peer);

  if (getpeername(s, (struct sockaddr *)&peer, &peer_len) == -1) return -1;

  sprintf(dest, 
	  "%d.%d.%d.%d",
	  peer.sin_addr.s_addr & 255,
	  (peer.sin_addr.s_addr >> 8) & 255,
	  (peer.sin_addr.s_addr >> 16) & 255,
	  (peer.sin_addr.s_addr >> 24) & 255);
    
  return 0;
  /* printf("Peer's port is: %d\n", (int) ntohs(peer.sin_port)); */
}


/* the following are used for select */
short ml_poll_rd = POLLIN; /* | POLLPRI */
short ml_poll_wr = POLLOUT;
/* XXX Cygwin uses POLLPRI instead of POLLERR */
#ifdef __CYGWIN__
short ml_poll_ex = POLLPRI;
#else
short ml_poll_ex = POLLERR;
#endif
short ml_poll_hup = POLLHUP;
short ml_poll_nval =  POLLNVAL;

char *ml_alloc_pollfds (int n)
{
  struct pollfd * p = (struct pollfd *) malloc (sizeof(struct pollfd) * n);
  if (p) bzero ((void *) p, sizeof(struct pollfd) * n);
  return (char *) p;
}

void ml_set_pollfds (struct pollfd * p, int i, int fd, short e)
{
  p[i].fd = fd;
  p[i].events = e;
}

short ml_get_pollfds (struct pollfd * p, int i)
{
  return p[i].revents;
}

/* on linux 2.4, the timeout argument is milliseconds.
   but I seem to recall switching between msec and usec.
   is it different on other platforms? 

              - Tom7    26 Jun 2007
*/
int ml_poll (struct pollfd * p, unsigned int n, int msec)
{
  int e;
  sigset_t blockme;

#if 0
  printf("[rnc] polling with timeout %d\n", msec);

  { 
    int i;
    for(i = 0; i < n; i ++) {
      printf("[ in] %2d = %2d (%04X) (%04X)\n",
	     i, p[i].fd, p[i].events, p[i].revents);
    }
  }
#endif

  if (sigfillset(&blockme))
    return -1;
  if (sigprocmask(SIG_BLOCK, &blockme, NULL))
    return -1;
  if ((e = poll (p, n, msec)) == -1)
    printf ("POLL ERROR: %s\n", strerror(errno));
  if (sigprocmask(SIG_UNBLOCK, &blockme, NULL))
    return -1;

  /* In some error conditions, poll will report
     events that we did not request. This causes
     problems with the network implementation, so
     mask them out here. */
  { 
    int i;
    for(i = 0; i < n; i ++) {
      p[i].revents &= (p[i].events |
		       POLLPRI | POLLERR |
		       POLLHUP | POLLNVAL);
    }
  }

#if 0
  {
    int i;
    for(i = 0; i < n; i ++) {
      printf("[out] %2d = %2d (%04X) (%04X)\n",
	     i, p[i].fd, p[i].events, p[i].revents);
    }
  }
#endif
  
  ml_errno = errno;
  return e;
}

int ml_test(int fd)
{
  return fcntl(fd, F_GETFD);
}
