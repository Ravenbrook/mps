/* class.h -- simple class system interface
 *
 * $Id$
 * Copyright (C) 2016 Ravenbrook Limited.  See end of file for license.
 */

#ifndef class_h
#define class_h

#include "mpmtypes.h"
#include "check.h"
#include "mpslib.h"


/* IsA -- subtype predicate
 *
 * An instance is a subtype of another type if its typeId is
 * divisible by the other type's typeId, because each type has
 * a unique prime.
 *
 * "Fast Dynamic Casting"; Michael Gibbs, Bjarne
 *  Stroustrup; 2004;
 *  <http://www.stroustrup.com/fast_dynamic_casting.pdf>.
 */

#define IsSub(class, subclass) ((subclass)->typeId % ClassTypeId ## class == 0)
#define IsA(class, inst) IsSub(class, (inst)->instClass)


/* MustBeA -- subtype or die */

#define XMUSTBEA(_class, inst, condstring) \
  ((inst) != NULL && \
   (inst)->instClass != NULL && \
   IsA(_class, inst) ? \
   (_class)(inst) : \
   (_class)mps_lib_assert_fail(MPS_FILE, __LINE__, condstring))

#define MustBeA(_class, inst) XMUSTBEA(_class, inst, "MustBeA " #_class ": " #inst)

#define MustBeSub(class, subclass) \
  (IsSub(class, subclass) ? \
   (class ## Class)subclass : \
   (class ## Class)mps_lib_assert_fail(MPS_FILE, __LINE__, "MustBeSub " #class ": " #subclass))

#define CouldBeA(_class, inst) ((_class)inst)


/* Checking macros.  These will move into check.h. */

#define TESTC(class, val)           IsA(class, val)

#define ASSERT_CLASSCHECK(class, val) \
  ASSERT(class ## Check(CouldBeA(Inst, val)), "ClassCheck " #class ": " #val)

#if defined(AVER_AND_CHECK_NONE)
#define AVERC(class, val)           DISCARD(class ## Check(MustBeA(Inst, val)))
#define CHECKC(class, val)          DISCARD(MustBeA(class, val))
#else
#define AVERC                       ASSERT_CLASSCHECK
#define CHECKC(class, val) \
  ASSERT(TESTC(class, val), "ClassCheck " #class ": " #val)
#endif

#if defined(AVER_AND_CHECK_ALL)
#define AVERC_CRITICAL              ASSERT_CLASSCHECK
#else
#define AVERC_CRITICAL(class, val)  DISCARD(class ## Check(MustBeA(Inst, val)))
#endif

#define CHECKDC(class, val) \
  CHECK_BY_LEVEL(NOOP, \
                 CHECKC(class, val), \
                 ASSERT_CLASSCHECK(class, val))


/* Method -- dynamic dispatch method call */

#define Method(_class, inst, meth) (MustBeSub(_class, (inst)->instClass)->meth)


/* CLASSES -- the table of classes
 *
 * FIXME: Static check of primality, etc.
 */

#define CLASSES(CLASS, X) \
  /*       ident     prime   super      doc */ \
  CLASS(X, Inst,         3,  NoSuper,   "base type of instances") \
  CLASS(X, InstClass,    5,  Inst,      "class of all instances") \
  CLASS(X, ClassClass,   7,  NoSuper,   "class of all classes") \
  CLASS(X, Land,        11,  Inst,      "set of address ranges") \
  CLASS(X, LandClass,   13,  InstClass, "class of lands") \
  CLASS(X, CBS,         17,  Land,      "coalescing block structure") \
  CLASS(X, CBSFast,     19,  CBS,       "CBS with size property") \
  CLASS(X, CBSZoned,    23,  CBSFast,   "CBSFast with zone seg property") \
  CLASS(X, Freelist,    27,  Land,      "freelist land") \
  CLASS(X, Failover,    29,  Land,      "failover land")


/* Declare types for all classes. */

#if 0
#define CLASS_TYPE(UNUSED, ident, prime, super, doc) \
  typedef struct ident ## Struct *ident;

CLASSES(CLASS_TYPE, UNUSED)
#endif

typedef struct InstStruct *Inst;
typedef struct InstClassStruct *InstClass;
typedef struct ClassClassStruct *ClassClass;


/* ClassPrimeEnum -- unique prime for each class
 *
 * This declares enum constants such as ClassPrimeInst, ClassPrimeClass, etc.
 */

#define CLASS_PRIME_ENUM(prefix, ident, prime, super, doc) prefix ## ident = prime,

typedef enum ClassPrimeEnum {
  CLASSES(CLASS_PRIME_ENUM, ClassPrime)
  ClassPrimeInvalid = 2
} ClassPrimeEnum;


/* ClassTypeIdEnum -- unique typeId for each class
 *
 * This declares enum constants such as ClassTypeIdInst, ClassTypeIdClass, etc.
 */

#define CLASS_TYPEID_ENUM(prefix, ident, prime, super, doc) \
  prefix ## ident = prime * ClassTypeId ## super,

typedef enum ClassTypeIdEnum {
  ClassTypeIdNoSuper = 1,
  CLASSES(CLASS_TYPEID_ENUM, ClassTypeId)
  ClassTypeIdInvalid = 2
} ClassTypeIdEnum;


/* Inst -- the base type of all instances */

#define InstFIELDS(FIELD, X) \
  FIELD(X, InstClass, instClass, "class of this instance")


/* Class -- the base class of all classes */

typedef const char *ClassName;
typedef unsigned ClassTypeId;
typedef Bool (*CheckMethod)(Inst inst);

#define InstClassFIELDS(FIELD, X) \
  InstFIELDS(FIELD, X) \
  FIELD(X, ClassName,   className, "human readable class name") \
  FIELD(X, ClassTypeId, prime,     "unique prime for this class") \
  FIELD(X, ClassTypeId, typeId,    "product of class prime and superclass typeId") \
  /* FIELD(X, InstClass,   super,     "superclass") */ \
  FIELD(X, CheckMethod, check,     "check consistency of instance")


/* ClassClass -- the class of all classes */

#define ClassClassFIELDS(FIELD, X) \
  InstClassFIELDS(FIELD, X)


/* Declare structure types for all classes. */

#define CLASS_STRUCT_FIELD(UNUSED, type, ident, doc) type ident;

#define CLASS_STRUCT(UNUSED, ident, prime, super, doc) \
  typedef struct ident ## Struct { \
    ident ## FIELDS(CLASS_STRUCT_FIELD, UNUSED) \
  } ident ## Struct;

#define CLASS_DEFSTRUCT(ident) CLASS_STRUCT(UNUSED, ident, UNUSED, UNUSED, UNUSED)

#if 0
CLASSES(CLASS_STRUCT, UNUSED)
#endif
CLASS_DEFSTRUCT(Inst)
CLASS_DEFSTRUCT(InstClass)
CLASS_DEFSTRUCT(ClassClass)


/* Declare check methods for all classes. */

#define CLASS_CHECK(UNUSED, ident, prime, super, doc) \
  extern Res ident ## Check(Inst inst);

CLASSES(CLASS_CHECK, UNUSED)


extern void InstClassInit(InstClass instClass);
extern Res InstInit(Inst inst);
extern void InstFinish(Inst inst);
#define ClassNameOfInst(inst) ((inst)->instClass->className)


#endif /* class_h */

/* C. COPYRIGHT AND LICENSE
 *
 * Copyright (C) 2013 Ravenbrook Limited <http://www.ravenbrook.com/>.
 * All rights reserved.  This is an open source license.  Contact
 * Ravenbrook for commercial licensing options.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are
 * met:
 * 
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 
 * 3. Redistributions in any form must be accompanied by information on how
 * to obtain complete source code for this software and any accompanying
 * software that uses this software.  The source code must either be
 * included in the distribution or be available for no more than the cost
 * of distribution plus a nominal fee, and must be freely redistributable
 * under reasonable conditions.  For an executable file, complete source
 * code means the source code for all modules it contains. It does not
 * include source code for modules or files that typically accompany the
 * major components of the operating system on which the executable file
 * runs.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR
 * PURPOSE, OR NON-INFRINGEMENT, ARE DISCLAIMED. IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS AND CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
