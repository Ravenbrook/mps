/*

 $Header: /project/cmucl/cvsroot/src/lisp/hppa-validate.h,v 1.5 2005/01/13 19:55:00 fgilham Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#ifndef _HPPA_VALIDATE_H_
#define _HPPA_VALIDATE_H_

#define READ_ONLY_SPACE_START   (0x20000000)
#define READ_ONLY_SPACE_SIZE    (0x08000000)

#define STATIC_SPACE_START	(0x28000000)
#define STATIC_SPACE_SIZE	(0x08000000)

#define DYNAMIC_0_SPACE_START	(0x30000000)
#define DYNAMIC_1_SPACE_START	(0x50000000)
#define DYNAMIC_SPACE_SIZE	(0x20000000)

#define CONTROL_STACK_START	(0x70000000)
#define CONTROL_STACK_SIZE	(0x00100000)

#define BINDING_STACK_START	(0x71000000)
#define BINDING_STACK_SIZE	(0x00100000)

#endif /* _HPPA_VALIDATE_H_ */
