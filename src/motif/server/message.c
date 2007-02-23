/*

 $Header: /project/cmucl/cvsroot/src/motif/server/message.c,v 1.6 2000/02/15 11:59:25 pw Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>
#include <X11/Intrinsic.h>

#include "global.h"
#include "message.h"


message_t pending=NULL;

message_t message_new(long serial) {
  message_t new;

  new = XtNew(struct message);
  new->next = NULL;
  new->packets = NULL;
  new->serial = serial;
  new->packet_count = 0;

  return new;
}

void message_free(message_t message) {
  packet_t last = message->packets;
  packet_t current = last;

  do {
    packet_t next = current->next;
    packet_free(current);
    current = next;
  } while( current != last );

  XtFree( (char *)message );
}

void message_add_packet(message_t message) {
  packet_t new;

  new = packet_new(message->serial,++message->packet_count);

  if( message->packets ) {
    new->next = message->packets->next;
    message->packets->next = new;
  }

  message->packets = new;
}


void message_insert_packet(message_t message, packet_t new) {
  packet_t scan=message->packets;

  /* Inserts in reverse order of sequence number (->current) */

  if( !scan )
    message->packets = new;
  else
    if( new->current > scan->current ) {
      message->packets = new;
      new->next = scan;
      scan->next =new;
    } else {
      do { scan = scan->next; } while( scan->next->current < new->current );
      new->next = scan->next;
      scan->next = new;
    }
  message->packet_count++;
}

packet_t nreverse_packet_list (packet_t list)
{
  packet_t this = list->next;
  packet_t next = list;
  packet_t last = NULL;
  while (this) {
    next->next = last;
    last = next;
    next = this;
    this = (this == list)? NULL : this->next;
  }
  list->next = last;
  return last;
}

unsigned char message_get_byte(message_t message) {
  packet_t packet=message->packets;

  if( (packet->fill-packet->data) >= packet->length )
    packet = message->packets = packet->next;

  return( packet_get_byte(packet) );
}

short message_get_word(message_t message) {
  packet_t packet=message->packets;

  if( (packet->fill-packet->data) >= (packet->length-1) )
    packet = message->packets = packet->next;

  return( packet_get_word(packet) );
}

long message_get_dblword(message_t message) {
  packet_t packet=message->packets;

  if( (packet->fill-packet->data) >= (packet->length-3) )
    packet = message->packets = packet->next;

  return( packet_get_dblword(packet) );
}


void message_put_byte(message_t message, unsigned char byte) {
  packet_t packet = message->packets;

  if( packet->length < PACKET_SIZE )
    packet_put_byte(packet,byte);
  else {
    message_add_packet(message);
    packet_put_byte(message->packets,byte);
  }
}

void message_put_word(message_t message, short word) {
  packet_t packet = message->packets;

  if( packet->length < PACKET_SIZE-1 )
    packet_put_word(packet,word);
  else {
    message_add_packet(message);
    packet_put_word(message->packets,word);
  }
}

void message_put_dblword(message_t message, long dword) {
  packet_t packet = message->packets;

  if( packet->length < PACKET_SIZE-3 )
    packet_put_dblword(packet,dword);
  else {
    message_add_packet(message);
    packet_put_dblword(message->packets, dword);
  }
}

void message_send(int socket, message_t message){
  message->packets = nreverse_packet_list(message->packets);
  {
    int total = message->packet_count;
    packet_t first = message->packets;
    packet_t current = first;

    do {
      current->total = total;
      packet_send(socket, current);
      
      current = current->next;
    } while( current != first );
  }
}


void kill_deferred_message(long serial) {
  message_t scan = pending;
  message_t target = NULL;

  if( scan && scan->serial == serial ) {
    target = scan;
    scan = target;
  } else
    while( scan->next ) {
      if( scan->next->serial == serial ) {
	target = scan->next;
	scan->next = target->next;
      }
      scan = scan->next;
    }

  target->next = NULL;
}


/*
* The given packet needs to be held over and joined with it's friends.
*/
message_t message_defer_packet(packet_t packet) {
  message_t current = pending;
  message_t target = NULL;

  while( current && !target ) {
    if( current->serial == packet->serial ) target=current;
    current=current->next;
  }

  if( !target ) {
    target = message_new(packet->serial);
    target->next = pending;
    pending = target;
  }

  message_insert_packet(target,packet);

  if( target->packet_count == packet->total ) {
    /*
    * It is assumed that single packet requests were handled elsewhere.
    * Thus, if this request is complete it must have been sitting on the
    * pending list.
    */
    current = pending;
    if( target == pending )
      pending = target->next;
    else {
      while( current->next != target ) current = current->next;
      current->next = target->next;
    }
    target->next = NULL;
    target->packets = nreverse_packet_list(target->packets);
    return target;
  } else
    return NULL;
}

message_t message_read(int socket) {
  message_t new;
  int count;

  while (1) {
    packet_t first = packet_new(0,1); /* The given serial is of course bogus */
    packet_read(socket,first);
    count=first->total;
    if( !first->total && !first->current ) {
      /* There is no client support for getting here yet! */
      kill_deferred_message(first->serial);
      if( global_will_trace ) {
	printf("Got cancellation for serial %d\n",first->serial);
	fflush(stdout);
	continue;}
    }
    if( count == 1 ) {
      new = message_new(next_serial++);
      new->packets = first;
      new->packet_count = 1;
      return new;
    }
    if(new = message_defer_packet(first)) /* intential assignment */
      return new;
  }
}

