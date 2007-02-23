/*
 * $Header: /project/cmucl/cvsroot/src/lisp/dynbind.c,v 1.4 2005/09/15 18:26:51 rtoy Exp $
 * 
 * Support for dynamic binding from C.
 */

#include "lisp.h"
#include "internals.h"
#include "globals.h"
#include "dynbind.h"

#if defined(ibmrt) || defined(i386) || defined(__x86_64)
#define GetBSP() ((struct binding *)SymbolValue(BINDING_STACK_POINTER))
#define SetBSP(value) SetSymbolValue(BINDING_STACK_POINTER, (lispobj)(value))
#else
#define GetBSP() ((struct binding *)current_binding_stack_pointer)
#define SetBSP(value) (current_binding_stack_pointer=(lispobj *)(value))
#endif

void
bind_variable(lispobj symbol, lispobj value)
{
    lispobj old_value;
    struct binding *binding;

    old_value = SymbolValue(symbol);
    binding = GetBSP();
    SetBSP(binding + 1);

    binding->value = old_value;
    binding->symbol = symbol;
    SetSymbolValue(symbol, value);
}

void
unbind(void)
{
    struct binding *binding;
    lispobj symbol;

    binding = GetBSP() - 1;

    symbol = binding->symbol;

    SetSymbolValue(symbol, binding->value);

    binding->symbol = 0;

    SetBSP(binding);
}

void
unbind_to_here(lispobj * bsp)
{
    struct binding *target = (struct binding *) bsp;
    struct binding *binding = GetBSP();
    lispobj symbol;

    while (target < binding) {
	binding--;

	symbol = binding->symbol;

	if (symbol) {
	    SetSymbolValue(symbol, binding->value);
	    binding->symbol = 0;
	}

    }
    SetBSP(binding);
}
