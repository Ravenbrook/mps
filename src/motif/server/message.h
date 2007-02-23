/*

 $Header: /project/cmucl/cvsroot/src/motif/server/message.h,v 1.2 1994/10/27 17:16:51 ram Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef MESSAGE_H
#define MESSAGE_H

#include "packet.h"


typedef struct message {
  struct message *next;
  long serial;
  int packet_count;
  packet_t packets;
} *message_t;

extern message_t message_new(long serial);
extern void message_free(message_t message);
extern void message_add_packet(message_t message);

extern unsigned char message_get_byte(message_t m);
extern short message_get_word(message_t m);
extern long message_get_dblword(message_t m);
extern void message_put_byte(message_t m, unsigned char c);
extern void message_put_word(message_t m, short w);
extern void message_put_dblword(message_t m, long d);

extern message_t message_read(int s);
extern void message_send(int s, message_t m);

#endif
