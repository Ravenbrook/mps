/*

 $Header: /project/cmucl/cvsroot/src/motif/server/packet.c,v 1.3 1994/10/27 17:16:51 ram Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>
#include <X11/Intrinsic.h>

#include "global.h"
#include "packet.h"


packet_t packet_new(long serial, short current) {
  packet_t new;

  new = XtNew(struct packet);
  new->data = XtMalloc(PACKET_SIZE);

  new->fill = new->data+HEADER_LENGTH; /* Advance past header */
  new->length = HEADER_LENGTH;         /* Length includes header */
  new->serial = serial;
  new->current = current;
  new->next = new;

  return new;
}

void packet_free(packet_t packet) {
  XtFree( (char *)packet->data );
  XtFree( (char *)packet );
}


/* Reads a byte from the packet buffer at the fill pointer */
unsigned char packet_get_byte(packet_t packet) {
  return( *(packet->fill++) );
}

short packet_get_word(packet_t packet) {
  char tmp[2];

  if( swap_bytes ) {
    tmp[1]=packet_get_byte(packet);
    tmp[0]=packet_get_byte(packet);
  } else {
    tmp[0] = packet_get_byte(packet);
    tmp[1] = packet_get_byte(packet);
  }

  return( *((short *)tmp) );
}

long packet_get_dblword(packet_t packet) {
  char tmp[4];
  int i;

  if( swap_bytes )
    for(i=3;i>=0;i--) tmp[i] = packet_get_byte(packet);
  else
    for(i=0;i<4;i++)  tmp[i] = packet_get_byte(packet);

  return( *((long *)tmp) );
}

void packet_put_byte(packet_t packet, unsigned char byte) {
  *(packet->fill++) = byte;
  packet->length++;
}

void packet_put_word(packet_t packet, short word) {
  char *tmp = (char *)&word;

  if( swap_bytes ) {
    packet_put_byte(packet,tmp[1]);
    packet_put_byte(packet,tmp[0]);
  } else {
    packet_put_byte(packet,tmp[0]);
    packet_put_byte(packet,tmp[1]);
  }
}

void packet_put_dblword(packet_t packet, long dword) {
  char *tmp = (char *)&dword;
  int i;

  if( swap_bytes )
    for(i=3;i>=0;i--) packet_put_byte(packet,tmp[i]);
  else
    for(i=0;i<4;i++)  packet_put_byte(packet,tmp[i]);
}

void packet_build_header(packet_t packet)
{
  int length = packet->length;

  packet->fill = packet->data;

  /* These operations will alter the packet length counter.  However, */
  /* we don't want that to happen, so we just preserve the length */
  /* temporarily and stick it back in when we're done.  This is sort */
  /* of a hack, but it works. */
  packet_put_dblword(packet,packet->serial);
  packet_put_word(packet,packet->current);
  packet_put_word(packet,packet->total);
  packet_put_dblword(packet,length);
  packet->length = length;
}

void packet_parse_header(packet_t packet) {
  packet->fill = packet->data;

  packet->serial  = packet_get_dblword(packet);
  packet->current = packet_get_word(packet);
  packet->total   = packet_get_word(packet);
  packet->length  = packet_get_dblword(packet);
}

void packet_send(int socket, packet_t packet) {
  int result, target;

  if( global_will_trace ) {
    printf("packet_send:  Sending packet --\n");
    printf("packet_send:     serial = %u\n",packet->serial);
    printf("packet_send:     length = %u\n",packet->length);
    printf("packet_send:     this is packet %d of %d\n",packet->current,
	   packet->total);
    fflush(stdout);
  }
  packet_build_header(packet);

  target = packet->length;
  packet->fill = packet->data;
  while( target ) {
    result = write(socket, packet->fill, target);
    if( result == -1 )
      fatal_error("packet_send:  Error in sending packet.");
    target -= result;
    packet->fill += result;
  }

}

void packet_read(int socket, packet_t packet) {
  int bytes_read = 0;
  int result,target;

  packet->fill = packet->data;

  while( bytes_read < HEADER_LENGTH ) {
    result = read(socket, packet->fill, HEADER_LENGTH-bytes_read);
    if( result == -1 )
      fatal_error("packet_read:  Error in reading packet header.");
    else if( result == 0 )
      fatal_error("packet_read:  Hit EOF while reading packet header.");
    bytes_read += result;
    packet->fill += result;
  }

  packet_parse_header(packet);
  target = packet->length-bytes_read;

  if( global_will_trace ) {
    printf("packet_read:  Receiving packet --\n");
    printf("packet_read:     serial = %u\n",packet->serial);
    printf("packet_read:     length = %u\n",packet->length);
    printf("packet_read:     This is packet %d of %d\n",packet->current,
	   packet->total);
    fflush(stdout);
  }

  while( target ) {
    result = read(socket, packet->fill, target);
    if( result == -1 )
      fatal_error("packet_read:  Error in reading packet body.");
    else if( result == 0 )
      fatal_error("packet_read:  Hit EOF while reading packet body.");
    target -= result;
    packet->fill += result;
  }

  packet->fill = packet->data+HEADER_LENGTH;
}
