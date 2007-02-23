/*

 $Header: /project/cmucl/cvsroot/src/motif/server/packet.h,v 1.2 1994/10/27 17:16:51 ram Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef PACKET_H
#define PACKET_H

typedef struct packet {
  struct packet *next;
  long length,serial;
  short current,total;
  char *data,*fill;
} *packet_t;

extern packet_t packet_new(long serial,short current);
extern void packet_free(packet_t packet);

extern unsigned char packet_get_byte(packet_t p);
extern short packet_get_word(packet_t p);
extern long packet_get_dblword(packet_t p);
extern void packet_put_byte(packet_t p, unsigned char b);
extern void packet_put_word(packet_t p, short w);
extern void packet_put_dblword(packet_t p, long d);

extern void packet_build_header(packet_t packet);
extern void packet_parse_header(packet_t packet);
extern void packet_send(int socket, packet_t packet);
extern void packet_read(int socket, packet_t packet);

#endif
