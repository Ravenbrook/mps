/* $Header: /project/cmucl/cvsroot/src/lisp/alloc.c,v 1.10 2005/09/15 18:26:50 rtoy Exp $ */

#include <string.h>

#include "lisp.h"
#include "internals.h"
#include "alloc.h"
#include "globals.h"
#include "gc.h"

#ifdef ibmrt
#define GET_FREE_POINTER() ((lispobj *)SymbolValue(ALLOCATION_POINTER))
#define SET_FREE_POINTER(new_value) \
    (SetSymbolValue(ALLOCATION_POINTER,(lispobj)(new_value)))
#define GET_GC_TRIGGER() ((lispobj *)SymbolValue(INTERNAL_GC_TRIGGER))
#define SET_GC_TRIGGER(new_value) \
    (SetSymbolValue(INTERNAL_GC_TRIGGER,(lispobj)(new_value)))
#else
#define GET_FREE_POINTER() current_dynamic_space_free_pointer
#define SET_FREE_POINTER(new_value) \
    (current_dynamic_space_free_pointer = (new_value))
#define GET_GC_TRIGGER() current_auto_gc_trigger
#define SET_GC_TRIGGER(new_value) \
    clear_auto_gc_trigger(); set_auto_gc_trigger(new_value);
#endif

#define ALIGNED_SIZE(n) (n+lowtag_Mask) & ~lowtag_Mask

/****************************************************************
Allocation Routines.
****************************************************************/

#if defined GENCGC
#define alloc(nbytes) alloc_pseudo_atomic(nbytes)
#include "gencgc.h"
#elif defined(WANT_CGC)
extern lispobj *alloc(int bytes);
#else
static lispobj *
alloc(int bytes)
{
    lispobj *result;

    /* Round to dual word boundry. */
    bytes = (bytes + lowtag_Mask) & ~lowtag_Mask;

    result = GET_FREE_POINTER();
    SET_FREE_POINTER(result + (bytes / sizeof(lispobj)));

    if (GET_GC_TRIGGER() && GET_FREE_POINTER() > GET_GC_TRIGGER()) {
	SET_GC_TRIGGER((char *) GET_FREE_POINTER()
		       - (char *) current_dynamic_space);
    }

    return result;
}
#endif

static lispobj *
alloc_unboxed(int type, int words)
{
    lispobj *result;

    result = (lispobj *) alloc(ALIGNED_SIZE((1 + words) * sizeof(lispobj)));

    *result = (lispobj) (words << type_Bits) | type;

    return result;
}

static lispobj
alloc_vector(int type, int length, int size)
{
    struct vector *result;

    result = (struct vector *)
	alloc(ALIGNED_SIZE((2 + (length * size + 31) / 32) * sizeof(lispobj)));

    result->header = type;
    result->length = make_fixnum(length);

    return ((lispobj) result) | type_OtherPointer;
}

lispobj
alloc_cons(lispobj car, lispobj cdr)
{
    struct cons *ptr = (struct cons *) alloc(ALIGNED_SIZE(sizeof(struct cons)));

    ptr->car = car;
    ptr->cdr = cdr;

    return (lispobj) ptr | type_ListPointer;
}

lispobj
alloc_number(long n)
{
    struct bignum *ptr;

#ifdef __x86_64
    if (-0x2000000000000000 < n && n < 0x2000000000000000)	/* -2^61 to 2^61 */
#else
    if (-0x20000000 < n && n < 0x20000000)
#endif
	return make_fixnum(n);
    else {
	ptr = (struct bignum *) alloc_unboxed(type_Bignum, 1);

	ptr->digits[0] = n;

	return (lispobj) ptr | type_OtherPointer;
    }
}

lispobj
alloc_string(char *str)
{
    int len = strlen(str);
    lispobj result = alloc_vector(type_SimpleString, len + 1, 8);
    struct vector *vec = (struct vector *) PTR(result);

    vec->length = make_fixnum(len);
    strcpy((char *) vec->data, str);

    return result;
}

lispobj
alloc_sap(void *ptr)
{
#ifndef alpha
    struct sap *sap_ptr = (struct sap *) alloc_unboxed(type_Sap, 1);
#else
    struct sap *sap_ptr = (struct sap *) alloc_unboxed(type_Sap, 3);
#endif
    sap_ptr->pointer = ptr;

    return (lispobj) sap_ptr | type_OtherPointer;
}
