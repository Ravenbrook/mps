/* impl.c.message: MPS / CLIENT MESSAGES
 *
 * $HopeName: MMsrc!message.c(MMdevel_drj_message.4) $
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
 * .name.clash: Sometimes in this module there are two functions
 * that you really want to give the same name.  An example is
 * MessageDeliver, this is both the function that dispatches to
 * the class specific deliver method and the function that
 * implements the external function mps_message_deliver.  They
 * have slightly different contracts but basically do the same
 * job.  I have generally called the function that implements the
 * external interface MessageExBlah.  drj 1997-08-19
 */



#include "mpm.h"


SRCID(message, "$HopeName: MMsrc!message.c(MMdevel_drj_message.4) $");


/* Maps from a Ring pointer to the message */
#define MessageNodeMessage(node) \
  PARENT(MessageStruct, queueRing, node)

/* commented out as it causes compiler warnings */
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

/* forward declarations */
static Bool MessageTypeEnabled(Space space, MessageType type);
static void MessageDelete(Message message);

/* is the message on the queue?
 * message on queue if and only if it's ring is not a singleton */
static Bool MessageOnQueue(Message message)
{
  AVERT(Message, message);

  return !RingIsSingle(&message->queueRing);
}


/* Checking Functions */


Bool MessageTypeCheck(MessageType type)
{
  CHECKL(type < MessageTypeMAX);

  return TRUE;
}


Bool MessageCheck(Message message)
{
  CHECKS(Message, message);
  CHECKU(Space, message->space);
  CHECKL(MessageTypeCheck(message->type));
  CHECKU(MessageClass, message->class);
  CHECKL(RingCheck(&message->queueRing));

  return TRUE;
}


Bool MessageClassCheck(MessageClass class)
{
  CHECKS(MessageClass, class);
  CHECKL(class->name != NULL);
  CHECKL(FUNCHECK(class->delete));
  CHECKL(class->endSig == MessageClassSig);

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
  AVER(!MessageOnQueue(message));
  if(MessageTypeEnabled(space, message->type)) {
    RingAppend(&space->messageRing, &message->queueRing);
  } else {
    /* discard message immediately if client hasn't enabled that type */
    MessageDiscard(space, message);
  }
}


/* returns the Message at the head of the queue */
static Message MessageHead(Space space)
{
  AVERT(Space, space);
  AVER(!RingIsSingle(&space->messageRing));

  return MessageNodeMessage(RingNext(&space->messageRing));
}


/* returns the type of a message */
MessageType MessageGetType(Message message)
{
  AVERT(Message, message);

  return message->type;
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
Bool MessageQueueType(MessageType *typeReturn, Space space)
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
  AVER(MessageTypeCheck(type));

  if(!MessagePoll(space)) {
    return FALSE;
  }
  message = MessageHead(space);
  if(MessageGetType(message) != type) {
    return FALSE;
  }

  return TRUE;
}


/* Discards a message
 * (called from external interface) */
void MessageDiscard(Space space, Message message)
{
  AVERT(Space, space);
  AVERT(Message, message);

  AVER(!MessageOnQueue(message));

  MessageDelete(message);
}


/* Deletes the message at the head of the queue.
 * Internal function. */
static void MessageDeleteHead(Space space)
{
  Message message;

  AVERT(Space, space);
  AVER(!RingIsSingle(&space->messageRing));

  message = MessageHead(space);
  AVERT(Message, message);
  RingRemove(&message->queueRing);
  MessageDelete(message);
}

/* Empties the queue by discarding all messages */
void MessageEmpty(Space space)
{
  AVERT(Space, space);

  while(!RingIsSingle(&space->messageRing)) {
    MessageDeleteHead(space);
  }
}

Bool MessageGet(Message *messageReturn, Space space, MessageType type)
{
  Message message;

  AVER(messageReturn != NULL);
  AVERT(Space, space);
  AVER(MessageTypeCheck(type));

  if(MessageHeadIsType(space, type)) {
    message = MessageHead(space);
    RingRemove(&message->queueRing);
    *messageReturn = message;
    return TRUE;
  }
  return FALSE;
}


static Bool MessageTypeEnabled(Space space, MessageType type)
{
  AVERT(Space, space);
  AVER(MessageTypeCheck(type));

  return BTGet(space->enabledMessageTypes, type);
}
  

void MessageTypeEnable(Space space, MessageType type)
{
  AVERT(Space, space);
  AVER(MessageTypeCheck(type));

  BTSet(space->enabledMessageTypes, type);
}



/* Dispatch Methods */


/* generic message delete dispatch */
static void MessageDelete(Message message)
{
  AVERT(Message, message);

  (*message->class->delete)(message);
}


/* type specific dispatch methods */

void MessageFinalizationRef(Ref *refReturn, Space space, Message message)
{
  AVER(refReturn != NULL);
  AVERT(Space, space);
  AVERT(Message, message);

  AVER(message->type == MessageTypeFinalization);

  (*message->class->finalizationRef)(refReturn, space, message);
}


/* type specific stub methods */

void MessageNoFinalizationRef(Ref *refReturn, Space space, Message message)
{
  AVER(refReturn != NULL);
  AVERT(Space, space);
  AVERT(Message, message);

  AVER(message->type == MessageTypeFinalization);

  NOTREACHED;
}
