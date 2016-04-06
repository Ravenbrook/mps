/* class.h -- simple class system interface
 *
 * $Id$
 * Copyright (C) 2016 Ravenbrook Limited.  See end of file for license.
 */

#ifndef class_h
#define class_h


/* CLASSES -- the table of classes */

#define CLASSES(CLASS, X) \
  /*       ident   prime  super  doc */ \
  CLASS(X, Inst,       3,  NONE, "base type of instances") \
  CLASS(X, Class,      5,  Inst, "base class of all classes")


/* Declare types for all classes. */

#define CLASS_TYPE(UNUSED, ident, prime, super, doc) \
  typedef struct ident ## Struct *ident;

CLASSES(CLASS_TYPE, UNUSED)


/* ClassPrimeEnum -- unique prime for each class
 *
 * This declares enum constants such as ClassPrimeInst, ClassPrimeClass, etc.
 */

#define CLASS_PRIME_ENUM(prefix, ident, prime, super, doc) prefix ## ident = prime,

typedef enum ClassPrimeEnum {
  CLASSES(CLASS_PRIME_ENUM, ClassPrime)
  ClassPrimeNONE = 1,
  ClassPrimeILLEGAL = 2
} ClassPrimeEnum;


/* ClassTypeIdEnum -- unique typeId for each class
 *
 * This declares enum constants such as ClassTypeIdInst, ClassTypeIdClass, etc.
 */

#define CLASS_TYPEID_ENUM(prefix, ident, prime, super, doc) \
  prefix ## ident = prime * ClassPrime ## super,

typedef enum ClassTypeIdEnum {
  CLASSES(CLASS_TYPEID_ENUM, ClassTypeId)
  ClassTypeIdNONE = 1
} ClassTypeIdEnum;


/* Inst -- the base type of all instances */

#define InstFIELDS(FIELD, X) \
  FIELD(X, Class, klass, "class of this instance")


/* Class -- the base class of all classes */

typedef const char *ClassName;
typedef unsigned ClassTypeId;
typedef Bool (*CheckMethod)(Inst inst);

#define ClassFIELDS(FIELD, X) \
  InstFIELDS(FIELD, X) \
  FIELD(X, ClassName,   className, "human readable class name") \
  FIELD(X, ClassTypeId, typeId,    "product of class prime and superclass typeId") \
  FIELD(X, CheckMethod, check,     "check consistency of instance")


/* Declare structure types for all classes. */

#define CLASS_STRUCT_FIELD(UNUSED, type, ident, doc) type ident;

#define CLASS_STRUCT(UNUSED, ident, prime, super, doc)   \
  typedef struct ident ## Struct { \
    ident ## FIELDS(CLASS_STRUCT_FIELD, UNUSED) \
  } ident ## Struct;

CLASSES(CLASS_STRUCT, UNUSED)


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
