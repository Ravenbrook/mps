/* impl.c.message: MPS / CLIENT MESSAGES
 *
 * $HopeName: MMsrc!message.c(MMdevel_drj_message.1) $
 * Copyright (C) 1997 The Harlequin Group Limited.  All Rights Reserved.
 *
 * READERSHIP
 *
 * .readership: Any MPS developer.
 *
 * DESIGN
 *
 * .design: See design.mps.message (it really exists).
 *
 * PURPOSE
 *
 * .purpose: Provide the generic part of the MPS / Client message
 * interface.  Messages are instances of Message Classes; much of the
 * "real work" goes on in the modules that provide the actual messages.
 *
 * NOTES
 *
 * Sometimes in this module there are two functions that you really
 * want to give the same name.  An example is MessageDeliver, this is
 * both the function that dispatches to the class specific deliver
 * method and the function that implements the external function
 * mps_message_deliver.  They have slightly different contracts but
 * basically do the same job.  I have generally called the function
 * that implements the external interface MessageExBlah.
 * drj 1997-08-19
 */


#include "mpm.h"


SRCID(message, "$HopeName: MMsrc!message.c(MMdevel_drj_message.1) $");


/* Maps from a Ring pointer to the message */
#define MessageNodeMessage(node) \
  PARENT(MessageStruct, queueRing, node)

#if 0
static Message (MessageNodeMessage)(Ring node)
{
  Message message;

  AVERT(Ring, node);
  message = MessageNodeMessage(node);
  AVERT(Message, message);

  return message;
}
#endif


/* Checking Functions */


Bool MessageCheck(Message message)
{
  CHECKS(Message, message);
  CHECKU(Space, message->space);
  CHECKU(MessageClass, message->class);
  CHECKL(RingCheck(&message->queueRing));

  return TRUE;
}


Bool MessageClassCheck(MessageClass class)
{
  CHECKS(MessageClass, class);
  CHECKL(class->name != NULL);
  CHECKL(FUNCHECK(class->type));
  CHECKL(FUNCHECK(class->deliver));
  CHECKL(FUNCHECK(class->delete));
  CHECKL(class->endSig == MessageClassSig);

  return TRUE;
}


Bool MessageTypeCheck(MessageType type)
{
  /* No check yet */

  return TRUE;
}


/* Internal Functions */


/* returns the space associated with a message */
Space MessageSpace(Message message)
{
  AVERT(Message, message);

  return message->space;
}


/* return the class of a message */
MessageClass MessageGetClass(Message message)
{
  AVERT(Message, message);

  return message->class;
}


/* Initialises a message */
void MessageInit(Space space, Message message, MessageClass class)
{
  AVERT(Space, space);
  /* we are initialising the message so we can't check it */
  AVERT(MessageClass, class);

  message->space = space;
  message->class = class;
  RingInit(&message->queueRing);
  message->sig = MessageSig;

  AVERT(Message, message);

  return;
}


/* Finishes a message */
void MessageFinish(Message message)
{
  AVERT(Message, message);
  AVER(RingIsSingle(&message->queueRing));

  message->sig = SigInvalid;
}


/* Posts a message to the space's queue of pending messages */
void MessagePost(Space space, Message message)
{
  AVERT(Space, space);
  AVERT(Message, message);

  /* queueRing field must be a singleton, see */
  /* design.mps.message.fun.post.singleton */
  AVER(RingIsSingle(&message->queueRing));
  RingAppend(&space->messageRing, &message->queueRing);

  return;
}


/* returns the Message at the head of the queue */
static Message MessageHead(Space space)
{
  AVERT(Space, space);
  AVER(!RingIsSingle(&space->messageRing));

  return MessageNodeMessage(RingNext(&space->messageRing));
}


/* Dispatch Methods */


/* returns the type of a message */
static MessageType MessageGetType(Message message)
{
  AVERT(Message, message);

  return (*message->class->type)(message);
}

/* delivers a message */
static void MessageDeliver(Message message, void *buffer, size_t length)
{
  AVERT(Message, message);
  AVER(buffer != NULL);
  AVER(length > 0);

  (*message->class->deliver)(message, buffer, length);
}

/* deletes a message */
static void MessageDelete(Message message)
{
  AVERT(Message, message);

  (*message->class->delete)(message);
}


/* External Functions
 *
 * These are actually the internal implementations of functions
 * exposed through the external interface */


/* Determines whether the queue has any messages on it */
Bool MessagePoll(Space space)
{
  AVERT(Space, space);

  if(RingIsSingle(&space->messageRing)) {
    return FALSE;
  } else {
    return TRUE;
  }
}


/* Determines the type of a message at the head of the queue */
Bool MessageExType(MessageType *typeReturn, Space space)
{
  Message message;
  MessageType type;

  AVER(typeReturn != NULL);
  AVERT(Space, space);

  if(!MessagePoll(space)) {
    return FALSE;
  }
  message = MessageHead(space);
  type = MessageGetType(message);
  *typeReturn = type;

  return TRUE;
}


/* Checks the type of the message at the head of the queue.
 *
 * returns FALSE if there is no message at the head of the queue
 * or if the type of the message at the head of the queue doesn't
 * match.
 *
 * Used internally by the implementations of the external
 * functions.
 */
static Bool MessageHeadIsType(Space space, MessageType type)
{
  Message message;

  AVERT(Space, space);
  AVERT(MessageType, type);

  if(!MessagePoll(space)) {
    return FALSE;
  }
  message = MessageHead(space);
  if(MessageGetType(message) != type) {
    return FALSE;
  }

  return TRUE;
}


/* Copies a message into a buffer */
Bool MessageExDeliver(Space space, MessageType type,
		    void *buffer, size_t length)
{
  Message message;

  AVERT(Space, space);
  AVERT(MessageType, type);
  AVER(buffer != NULL);
  /* there is a strict relation between length and type, */
  /* but not one that this function is in a position to know */
  AVER(length > 0);

  if(!MessageHeadIsType(space, type)) {
    return FALSE;
  }
  message = MessageHead(space);
  MessageDeliver(message, buffer, length);
  RingRemove(&message->queueRing);
  MessageDelete(message);

  return TRUE;
}

Bool MessageDiscard(Space space, MessageType type)
{
  Message message;
  AVERT(Space, space);
  AVERT(MessageType, type);

  if(!MessageHeadIsType(space, type)) {
    return FALSE;
  }
  message = MessageHead(space);
  RingRemove(&message->queueRing);
  MessageDelete(message);

  return TRUE;
}
