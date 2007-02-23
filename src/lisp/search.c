/*

 $Header: /project/cmucl/cvsroot/src/lisp/search.c,v 1.4 2005/09/15 18:26:52 rtoy Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <string.h>
#include "lisp.h"
#include "internals.h"
#include "os.h"
#include "search.h"

boolean
search_for_type(int type, lispobj ** start, int *count)
{
    lispobj obj, *addr;

    while ((*count == -1 || (*count > 0)) &&
	   valid_addr((os_vm_address_t) * start)) {
	obj = **start;
	addr = *start;
	if (*count != -1)
	    *count -= 2;

	if (TypeOf(obj) == type)
	    return TRUE;

	(*start) += 2;
    }
    return FALSE;
}


boolean
search_for_symbol(char *name, lispobj ** start, int *count)
{
    struct symbol *symbol;
    struct vector *symbol_name;

    while (search_for_type(type_SymbolHeader, start, count)) {
	symbol = (struct symbol *) PTR((lispobj) * start);
	if (LowtagOf(symbol->name) == type_OtherPointer) {
	    symbol_name = (struct vector *) PTR(symbol->name);
	    if (valid_addr((os_vm_address_t) symbol_name) &&
		TypeOf(symbol_name->header) == type_SimpleString &&
		strcmp((char *) symbol_name->data, name) == 0)
		return TRUE;
	}
	(*start) += 2;
    }
    return FALSE;
}
