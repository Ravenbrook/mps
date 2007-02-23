/*
 * $Header: /project/cmucl/cvsroot/src/lisp/interr.c,v 1.8 2005/09/15 18:26:51 rtoy Exp $
 *
 * Stuff to handle internal errors.
 *
 */

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>

#include "arch.h"
#include "signal.h"

#include "lisp.h"
#include "internals.h"
#include "interr.h"
#include "print.h"
#include "lispregs.h"


/* Lossage handler. */

static void
default_lossage_handler(void)
{
    exit(1);
}

static void (*lossage_handler) (void) = default_lossage_handler;

void
set_lossage_handler(void handler(void))
{
    lossage_handler = handler;
}

void
lose(char *fmt, ...)
{
    va_list ap;

    if (fmt != NULL) {
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	fflush(stderr);
	va_end(ap);
    }
    lossage_handler();
}


/* Internal error handler for when the Lisp error system doesn't exist. */

static char *errors[] = ERRORS;

void
internal_error(os_context_t * context)
{
    unsigned char *ptr = arch_internal_error_arguments(context);
    int len, scoffset, sc, offset, ch;

    len = *ptr++;
    printf("Error: %s\n", errors[*ptr++]);
    len--;
    while (len > 0) {
	scoffset = *ptr++;
	len--;
	if (scoffset == 253) {
	    scoffset = *ptr++;
	    len--;
	} else if (scoffset == 254) {
	    scoffset = ptr[0] + ptr[1] * 256;
	    ptr += 2;
	    len -= 2;
	} else if (scoffset == 255) {
	    scoffset = ptr[0] + (ptr[1] << 8) + (ptr[2] << 16) + (ptr[3] << 24);
	    ptr += 4;
	    len -= 4;
	}
	sc = scoffset & 0x1f;
	offset = scoffset >> 5;

	printf("    SC: %d, Offset: %d", sc, offset);
	switch (sc) {
	  case sc_AnyReg:
	  case sc_DescriptorReg:
	      putchar('\t');
	      brief_print(SC_REG(context, offset));
	      break;

	  case sc_BaseCharReg:
	      ch = SC_REG(context, offset);
#ifdef i386
	      if (offset & 1)
		  ch = ch >> 8;
	      ch = ch & 0xff;
#endif
	      switch (ch) {
		case '\n':
		    printf("\t'\\n'\n");
		    break;
		case '\b':
		    printf("\t'\\b'\n");
		    break;
		case '\t':
		    printf("\t'\\t'\n");
		    break;
		case '\r':
		    printf("\t'\\r'\n");
		    break;
		default:
		    if (ch < 32 || ch > 127)
			printf("\\%03o", ch);
		    else
			printf("\t'%c'\n", ch);
		    break;
	      }
	      break;
	  case sc_SapReg:
#ifdef sc_WordPointerReg
	  case sc_WordPointerReg:
#endif
	      printf("\t0x%08x\n", SC_REG(context, offset));
	      break;
	  case sc_SignedReg:
	      printf("\t%d\n", SC_REG(context, offset));
	      break;
	  case sc_UnsignedReg:
	      printf("\t%u\n", SC_REG(context, offset));
	      break;
#if 0				/* broken */
#ifdef sc_SingleReg
	  case sc_SingleReg:
	      printf("\t%g\n", *(float *) &context->sc_fpregs[offset]);
	      break;
#endif
#ifdef sc_DoubleReg
	  case sc_DoubleReg:
	      printf("\t%g\n", *(double *) &context->sc_fpregs[offset]);
	      break;
#endif
#ifdef sc_LongReg
	  case sc_LongReg:
	      printf("\t%Lg\n", *(long double *) &context->sc_fpregs[offset]);
	      break;
#endif
#endif
	  default:
	      printf("\t???\n");
	      break;
	}
    }

    lose(NULL);
}




/* Utility routines used by random pieces of code. */

lispobj
debug_print(lispobj string)
{
    printf("%s\n", (char *) (((struct vector *) PTR(string))->data));
    fflush(stdout);

    return NIL;
}
