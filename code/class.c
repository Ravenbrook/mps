/* class.c -- simple class system implementation
 *
 * $Id$
 * Copyright (C) 2016 Ravenbrook Limited.  See end of file for license.
 */

#include "class.h"
#include "mpmtypes.h"
#include "check.h"
#include "mpm.h" /* for FUNCHECK */


static InstClassStruct classInstClassStruct;
static InstClass classInstClass = NULL;

static struct ClassClassStruct classClassClassStruct;
static ClassClass classClassClass = NULL;


/* InstClassInvalid -- the invalid class
 *
 * When an instance is finished its class pointer is changed to this
 * class, to guard against it being used again by mistake.
 */

#define InstClassInvalid &InstClassInvalidStruct

static Bool InstNoCheck(Inst inst)
{
  UNUSED(inst);
  NOTREACHED;
  return FALSE;
}

static void InstNoFinish(Inst inst)
{
  UNUSED(inst);
  NOTREACHED;
}

static InstClassStruct InstClassInvalidStruct = {
  InstClassInvalid,
  "invalid class",
  ClassPrimeInvalid,
  ClassTypeIdInvalid,
  InstNoCheck,
  InstDescribe, /* it's still possible to describe one */
  InstNoFinish
};


/* InstClassCheck -- check whether object is a instance class */

Bool InstClassCheck(Inst inst)
{
  InstClass instClass = MustBeA(InstClass, inst);
  CHECKL(instClass->className != NULL);
  /* CHECKL(IsPrime(klass->prime)); */
  CHECKL(instClass->typeId % instClass->prime == 0);
  InstClassMETHODS(CHECKM, instClass);
  return TRUE;
}


/* ClassDescribe -- describe a class */

Res ClassDescribe(Inst inst, mps_lib_FILE *stream, Count depth)
{
  InstClass class = CouldBeA(InstClass, inst);
  Res res;

  if (!TESTC(InstClass, class))
    return ResPARAM;
  if (stream == NULL)
    return ResPARAM;
  
  res = InstDescribe(inst, stream, depth);
  if (res != ResOK)
    return res;

  return WriteF(stream, depth + 2,
                "name   \"$S\"\n", (WriteFS)class->className,
                "prime  $U\n",     (WriteFU)class->prime,
                "typeId $U\n",     (WriteFU)class->typeId,
                NULL);
}


/* ClassClassInit -- initialise a class of classes */

static void ClassClassInit(ClassClass classClass)
{
  AVER(classClass != NULL);
  classClass->instClass = CouldBeA(InstClass, classClass); /* special little knot */
  classClass->className = "Class";
  classClass->prime = ClassPrimeInstClass;
  classClass->typeId = ClassTypeIdInstClass;
  /* classClass->super = &classInstClassStruct; */
  classClass->check = InstClassCheck;
  classClass->describe = ClassDescribe;
  classClass->finish = InstNoFinish;
  AVERC(InstClass, classClass);
}


Bool InstCheck(Inst inst)
{
  (void)MustBeA(Inst, inst);
  return TRUE;
}

Res InstDescribe(Inst inst, mps_lib_FILE *stream, Count depth)
{
  if (!TESTC(Inst, inst))
    return ResPARAM;
  if (stream == NULL)
    return ResPARAM;

  return WriteF(stream, depth, "$S $P\n",
                (WriteFS)ClassNameOfInst(inst),
                (WriteFP)inst,
                NULL);
}


/* InstClassInit -- initialise the class of instances */

void InstClassInit(InstClass instClass)
{
  AVER(instClass != NULL);

  if (classClassClass == NULL) {
    ClassClassInit(&classClassClassStruct);
    classClassClass = &classClassClassStruct;
  }

  instClass->instClass = MustBeA(InstClass, classClassClass);
  instClass->className = "Inst";
  instClass->prime = ClassPrimeInst;
  instClass->typeId = ClassTypeIdInst;
  /* instClass->super = NULL; */
  instClass->check = InstCheck;
  instClass->describe = InstDescribe;
  instClass->finish = InstFinish;

  AVERC(InstClass, instClass);
}


/* InstInit -- initialize a base instance */

Res InstInit(Inst inst)
{
  if (classInstClass == NULL) {
    InstClassInit(&classInstClassStruct);
    classInstClass = &classInstClassStruct;
  }

  AVER(inst != NULL);
  inst->instClass = classInstClass;

  AVERC(Inst, inst);
  return ResOK;
}

void InstFinish(Inst inst)
{
  AVERC(Inst, inst);
  inst->instClass = InstClassInvalid;
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
