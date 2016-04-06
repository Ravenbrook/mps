/* class.c -- simple class system implementation
 *
 * $Id$
 * Copyright (C) 2016 Ravenbrook Limited.  See end of file for license.
 */

#include "class.h"
#include "mpmtypes.h"
#include "check.h"
#include "mpm.h" /* for FUNCHECK */


/* ClassCheck -- check whether object is a valid class */

Bool ClassCheck(Inst inst)
{
  Class klass = MustBeA(Class, inst);
  CHECKL(klass->className != NULL);
  /* CHECKL(IsPrime(klass->prime)); */
  CHECKL(klass->typeId % klass->prime == 0);
  CHECKL(FUNCHECK(klass->check));
  return TRUE;
}


/* ClassClassInit -- initialise a class of classes */

static Res ClassClassInit(ClassClass klass)
{
  AVER(klass != NULL);
  klass->klass = MustBeA(Class, klass); /* special little knot */
  klass->className = "Class";
  klass->prime = ClassPrimeClass;
  klass->typeId = ClassTypeIdClass;
  klass->check = ClassCheck;
  AVERT(Class, MustBeA(Inst, klass));
  return ResOK;
}


Bool InstCheck(Inst inst)
{
  (void)MustBeA(Inst, inst);
  return TRUE;
}


/* ClassInit -- initialise the base class of objects */

Res ClassInit(Class klass)
{
  static struct ClassClassStruct classClassStruct;
  static ClassClass classClass = NULL;

  AVER(klass != NULL);

  if (classClass == NULL) {
    ClassClassInit(&classClassStruct);
    classClass = &classClassStruct;
  }

  klass->klass = MustBeA(Class, classClass);
  klass->className = "Inst";
  klass->prime = ClassPrimeInst;
  klass->typeId = ClassTypeIdInst;
  klass->check = InstCheck;

  AVERT(Class, MustBeA(Inst, klass));
  return ResOK;
}


/* InstInit -- initialize a base instance */

Res InstInit(Inst inst)
{
  static ClassStruct classStruct;
  static Class klass = NULL;

  if (klass == NULL) {
    Res res = ClassInit(&classStruct);
    if (res != ResOK)
      return res;
    klass = &classStruct;
  }

  AVER(inst != NULL);
  inst->klass = klass;

  AVERT(Inst, inst);
  return ResOK;
}


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
