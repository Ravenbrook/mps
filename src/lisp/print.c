/* $Header: /project/cmucl/cvsroot/src/lisp/print.c,v 1.22 2006/07/12 18:42:40 rtoy Exp $ */

#include <stdio.h>
#include <string.h>
#include "print.h"
#include "lisp.h"
#include "internals.h"
#include "monitor.h"
#include "vars.h"
#include "os.h"

static int max_lines = 20, cur_lines = 0;
static int max_depth = 5, brief_depth = 2, cur_depth = 0;
static int max_length = 5;
static boolean dont_decend = FALSE, skip_newline = FALSE;
static int cur_clock = 0;

static void print_obj(char *prefix, lispobj obj);

#define NEWLINE if (continue_p(TRUE)) newline(NULL); else return;

char *lowtag_Names[] = {
    "even fixnum",
    "function pointer",
    "other immediate [0]",
    "list pointer",
    "odd fixnum",
    "instance pointer",
    "other immediate [1]",
    "other pointer"
};

char *subtype_Names[] = {
    "unused 0",
    "unused 1",
    "bignum",
    "ratio",
    "single float",
    "double float",
#ifdef type_LongFloat
    "long float",
#endif
#ifdef type_DoubleDoubleFloat
    "double-double float",
#endif    
    "complex",
#ifdef type_ComplexSingleFloat
    "complex single float",
#endif
#ifdef type_ComplexDoubleFloat
    "complex double float",
#endif
#ifdef type_ComplexLongFloat
    "complex long float",
#endif
#ifdef type_ComplexDoubleDoubleFloat
    "complex double-double float",
#endif
    "simple-array",
    "simple-string",
    "simple-bit-vector",
    "simple-vector",
    "(simple-array (unsigned-byte 2) (*))",
    "(simple-array (unsigned-byte 4) (*))",
    "(simple-array (unsigned-byte 8) (*))",
    "(simple-array (unsigned-byte 16) (*))",
    "(simple-array (unsigned-byte 32) (*))",
#ifdef type_SimpleArraySignedByte8
    "(simple-array (signed-byte 8) (*))",
#endif
#ifdef type_SimpleArraySignedByte16
    "(simple-array (signed-byte 16) (*))",
#endif
#ifdef type_SimpleArraySignedByte30
    "(simple-array fixnum (*))",
#endif
#ifdef type_SimpleArraySignedByte32
    "(simple-array (signed-byte 32) (*))",
#endif
    "(simple-array single-float (*))",
    "(simple-array double-float (*))",
#ifdef type_SimpleArrayLongFloat
    "(simple-array long-float (*))",
#endif
#ifdef type_SimpleArrayDoubleDoubleFloat
    "(simple-array double-double-float (*))",
#endif
#ifdef type_SimpleArrayComplexSingleFloat
    "(simple-array (complex single-float) (*))",
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
    "(simple-array (complex double-float) (*))",
#endif
#ifdef type_SimpleArrayComplexLongFloat
    "(simple-array (complex long-float) (*))",
#endif
#ifdef type_SimpleArrayComplexDoubleDoubleFloat
    "(simple-array (complex double-double-float) (*))",
#endif
    "complex-string",
    "complex-bit-vector",
    "(array * (*))",
    "array",
    "code header",
    "function header",
    "closure header",
    "funcallable-instance header",
    "byte code function",
    "byte code closure",
/*    "unused function header 3",*/
    "closure function header",
    "return PC header",
    "value cell header",
    "symbol header",
    "character",
    "SAP",
    "unbound marker",
    "weak pointer",
    "instance header",
    "fdefn"
#ifdef type_ScavengerHook
	, "scavenger hook"
#endif
};

static void
indent(int in)
{
    static char *spaces =

	"                                                                ";

    while (in > 64) {
	fputs(spaces, stdout);
	in -= 64;
    }
    if (in != 0)
	fputs(spaces + 64 - in, stdout);
}

static boolean
continue_p(boolean newline)
{
    char buffer[256];

    if (cur_depth >= max_depth || dont_decend)
	return FALSE;

    if (newline) {
	if (skip_newline)
	    skip_newline = FALSE;
	else
	    putchar('\n');

	if (cur_lines >= max_lines) {
	    printf("More? [y] ");
	    fflush(stdout);

	    fgets(buffer, sizeof(buffer), stdin);

	    if (buffer[0] == 'n' || buffer[0] == 'N')
		throw_to_monitor();
	    else
		cur_lines = 0;
	}
    }

    return TRUE;
}

static void
newline(char *label)
{
    cur_lines++;
    if (label != NULL)
	fputs(label, stdout);
    putchar('\t');
    indent(cur_depth * 2);
}


static void
brief_fixnum(lispobj obj)
{
#ifndef alpha
    printf("%ld", ((long) obj) >> 2);
#else
    printf("%d", ((s32) obj) >> 2);
#endif
}

static void
print_fixnum(lispobj obj)
{
#ifndef alpha
    printf(": %ld", ((long) obj) >> 2);
#else
    printf(": %d", ((s32) obj) >> 2);
#endif
}

static void
brief_otherimm(lispobj obj)
{
    int type, c, idx;
    char buffer[10];

    type = TypeOf(obj);
    switch (type) {
      case type_BaseChar:
	  c = (obj >> 8) & 0xff;
	  switch (c) {
	    case '\0':
		printf("#\\Null");
		break;
	    case '\n':
		printf("#\\Newline");
		break;
	    case '\b':
		printf("#\\Backspace");
		break;
	    case '\177':
		printf("#\\Delete");
		break;
	    default:
		strcpy(buffer, "#\\");
		if (c >= 128) {
		    strcat(buffer, "m-");
		    c -= 128;
		}
		if (c < 32) {
		    strcat(buffer, "c-");
		    c += '@';
		}
		printf("%s%c", buffer, c);
		break;
	  }
	  break;

      case type_UnboundMarker:
	  printf("<unbound marker>");
	  break;

      default:
	  idx = type >> 2;
	  if (idx < (sizeof(subtype_Names) / sizeof(char *)))
	      printf("%s", subtype_Names[idx]);

	  else
	      printf("unknown type (0x%0x)", type);
	  break;
    }
}

static void
print_otherimm(lispobj obj)
{
    int type, idx;

    type = TypeOf(obj);
    idx = type >> 2;

    if (idx < (sizeof(subtype_Names) / sizeof(char *)))
	printf(", %s", subtype_Names[idx]);

    else
	printf(", unknown type (0x%0x)", type);

    switch (TypeOf(obj)) {
      case type_BaseChar:
	  printf(": ");
	  brief_otherimm(obj);
	  break;

      case type_Sap:
      case type_UnboundMarker:
	  break;

      default:
	  printf(": data=%ld", (obj >> 8) & 0xffffff);
	  break;
    }
}

static void
brief_list(lispobj obj)
{
    int space = FALSE;
    int length = 0;

    if (!valid_addr((os_vm_address_t) obj))
	printf("(invalid address)");
    else if (obj == NIL)
	printf("NIL");
    else {
	putchar('(');
	while (LowtagOf(obj) == type_ListPointer) {
	    struct cons *cons = (struct cons *) PTR(obj);

	    if (space)
		putchar(' ');
	    if (++length >= max_length) {
		printf("...");
		obj = NIL;
		break;
	    }
	    print_obj(NULL, cons->car);
	    obj = cons->cdr;
	    space = TRUE;
	    if (obj == NIL)
		break;
	}
	if (obj != NIL) {
	    printf(" . ");
	    print_obj(NULL, obj);
	}
	putchar(')');
    }
}

static void
print_list(lispobj obj)
{
    if (!valid_addr((os_vm_address_t) obj))
	printf("(invalid address)");
    else if (obj == NIL)
	printf(" (NIL)");
    else {
	struct cons *cons = (struct cons *) PTR(obj);

	print_obj("car: ", cons->car);
	print_obj("cdr: ", cons->cdr);
    }
}

static void
brief_struct(lispobj obj)
{
    printf("#<ptr to 0x%08lx instance>",
	   ((struct instance *) PTR(obj))->slots[0]);
}

static void
print_struct(lispobj obj)
{
    struct instance *instance = (struct instance *) PTR(obj);
    int i;
    char buffer[16];

    print_obj("type: ", ((struct instance *) PTR(obj))->slots[0]);
    for (i = 1; i < HeaderValue(instance->header); i++) {
	sprintf(buffer, "slot %d: ", i);
	print_obj(buffer, instance->slots[i]);
    }
}

static void
brief_otherptr(lispobj obj)
{
    lispobj *ptr, header;
    int type;
    struct symbol *symbol;
    struct vector *vector;
    char *charptr;

    ptr = (lispobj *) PTR(obj);

    if (!valid_addr((os_vm_address_t) obj)) {
	printf("(invalid address)");
	return;
    }

    header = *ptr;
    type = TypeOf(header);
    switch (type) {
      case type_SymbolHeader:
	  symbol = (struct symbol *) ptr;
	  vector = (struct vector *) PTR(symbol->name);
	  for (charptr = (char *) vector->data; *charptr != '\0'; charptr++) {
	      if (*charptr == '"')
		  putchar('\\');
	      putchar(*charptr);
	  }
	  break;

      case type_SimpleString:
	  vector = (struct vector *) ptr;
	  putchar('"');
	  for (charptr = (char *) vector->data; *charptr != '\0'; charptr++) {
	      if (*charptr == '"')
		  putchar('\\');
	      putchar(*charptr);
	  }
	  putchar('"');
	  break;

      default:
	  printf("#<ptr to ");
	  brief_otherimm(header);
	  putchar('>');
    }
}

static void
print_slots(char **slots, int count, lispobj * ptr)
{
    while (count-- > 0)
	if (*slots)
	    print_obj(*slots++, *ptr++);
	else
	    print_obj("???: ", *ptr++);
}

static char *symbol_slots[] = { "value: ", "hash: ",
    "plist: ", "name: ", "package: ", NULL
};
static char *ratio_slots[] = { "numer: ", "denom: ", NULL };
static char *complex_slots[] = { "real: ", "imag: ", NULL };
static char *code_slots[] = { "words: ", "entry: ", "debug: ", NULL };
static char *array_slots[] = { "fill-pointer:   ",
    "fill-pointer-p: ",
    "elements:       ",
    "data:           ",
    "displacement:   ",
    "displaced-p:    ",
    /* Some reasonable number of dimensions */
    "dimension 1:    ",
    "dimension 2:    ",
    "dimension 3:    ",
    "dimension 4:    ",
    "dimension 5:    ",
    "dimension 6:    ",
    "dimension 7:    ",
    NULL
};


#if (defined(i386) || defined(__x86_64))
static char *fn_slots[] =
    { "inst start: ", "next: ", "name: ", "arglist: ", "type: ", NULL };
#else
static char *fn_slots[] =
    { "self: ", "next: ", "name: ", "arglist: ", "type: ", NULL };
#endif

static char *closure_slots[] = { "fn: ", NULL };
static char *funcallable_instance_slots[] =
    { "fn: ", "lexenv: ", "layout: ", NULL };
static char *weak_pointer_slots[] = { "value: ", "broken: ",
#ifdef GENCGC
    "mark-bit: ",
#endif
    NULL
};
static char *fdefn_slots[] = { "name: ", "function: ", "raw_addr: ", NULL };
static char *value_cell_slots[] = { "value: ", NULL };

#ifdef type_ScavengerHook
static char *scavenger_hook_slots[] =

    { "value: ", "function: ", "next: ", NULL };
#endif

static void
print_otherptr(lispobj obj)
{
    if (!valid_addr((os_vm_address_t) obj))
	printf("(invalid address)");
    else {
#ifndef alpha
	unsigned long *ptr;
	unsigned long header;
	unsigned long length;
#else
	u32 *ptr;
	u32 header;
	u32 length;
#endif
	int count, type, index;
	char *cptr, buffer[16];

#ifndef alpha
	ptr = (unsigned long *) PTR(obj);
	if (ptr == (unsigned long *) NULL) {
#else
	ptr = (u32 *) PTR(obj);
	if (ptr == (u32 *) NULL) {
#endif
	    printf(" (NULL Pointer)");
	    return;
	}

	header = *ptr++;
	length = (*ptr) >> 2;
	count = header >> 8;
	type = TypeOf(header);

	print_obj("header: ", header);
	if (LowtagOf(header) != type_OtherImmediate0
	    && LowtagOf(header) != type_OtherImmediate1) {
	    NEWLINE;
	    printf("(invalid header object)");
	    return;
	}

	switch (type) {
	  case type_Bignum:
	      ptr += count;
	      NEWLINE;
	      printf("0x");
	      while (count-- > 0)
		  printf("%08lx", *--ptr);
	      break;

	  case type_Ratio:
	      print_slots(ratio_slots, count, ptr);
	      break;

	  case type_Complex:
	      print_slots(complex_slots, count, ptr);
	      break;

	  case type_SymbolHeader:
	      print_slots(symbol_slots, count, ptr);
	      break;

	  case type_SingleFloat:
	      NEWLINE;
	      printf("%g", ((struct single_float *) PTR(obj))->value);
	      break;

	  case type_DoubleFloat:
	      NEWLINE;
	      printf("%g", ((struct double_float *) PTR(obj))->value);
	      break;

#ifdef type_LongFloat
	  case type_LongFloat:
	      NEWLINE;
	      printf("%Lg", ((struct long_float *) PTR(obj))->value);
	      break;
#endif

#ifdef type_DoubleDoubleFloat
          case type_DoubleDoubleFloat:
              NEWLINE;
              printf("%g %g", ((struct double_double_float *) PTR(obj))->hi,
                     ((struct double_double_float *) PTR(obj))->lo);
              break;
#endif              

#ifdef type_ComplexSingleFloat
	  case type_ComplexSingleFloat:
	      NEWLINE;
	      printf("%g", ((struct complex_single_float *) PTR(obj))->real);
	      NEWLINE;
	      printf("%g", ((struct complex_single_float *) PTR(obj))->imag);
	      break;
#endif

#ifdef type_ComplexDoubleFloat
	  case type_ComplexDoubleFloat:
	      NEWLINE;
	      printf("%g", ((struct complex_double_float *) PTR(obj))->real);
	      NEWLINE;
	      printf("%g", ((struct complex_double_float *) PTR(obj))->imag);
	      break;
#endif

#ifdef type_ComplexLongFloat
	  case type_ComplexLongFloat:
	      NEWLINE;
	      printf("%Lg", ((struct complex_long_float *) PTR(obj))->real);
	      NEWLINE;
	      printf("%Lg", ((struct complex_long_float *) PTR(obj))->imag);
	      break;
#endif

#ifdef type_ComplexDoubleDoubleFloat
	  case type_ComplexDoubleDoubleFloat:
	      NEWLINE;
	      printf("%g %g", ((struct complex_double_double_float *) PTR(obj))->real_hi,
                     ((struct complex_double_double_float *) PTR(obj))->real_lo);
	      NEWLINE;
	      printf("%g %g", ((struct complex_double_double_float *) PTR(obj))->imag_hi,
                     ((struct complex_double_double_float *) PTR(obj))->imag_lo);
	      break;
#endif


	  case type_SimpleString:
	      NEWLINE;
	      cptr = (char *) (ptr + 1);
	      putchar('\"');
	      while (length-- > 0)
		  putchar(*cptr++);
	      putchar('\"');
	      break;

	  case type_SimpleVector:
	      NEWLINE;
	      printf("length = %ld", length);
	      ptr++;
	      index = 0;
	      while (length-- > 0) {
		  sprintf(buffer, "%d: ", index++);
		  print_obj(buffer, *ptr++);
	      }
	      break;

	  case type_InstanceHeader:
	      NEWLINE;
	      printf("length = %d", count);
	      index = 0;
	      while (count-- > 0) {
		  sprintf(buffer, "%d: ", index++);
		  print_obj(buffer, *ptr++);
	      }
	      break;

	  case type_SimpleArray:
	  case type_SimpleBitVector:
	  case type_SimpleArrayUnsignedByte2:
	  case type_SimpleArrayUnsignedByte4:
	  case type_SimpleArrayUnsignedByte8:
	  case type_SimpleArrayUnsignedByte16:
	  case type_SimpleArrayUnsignedByte32:
#ifdef type_SimpleArraySignedByte8
	  case type_SimpleArraySignedByte8:
#endif
#ifdef type_SimpleArraySignedByte16
	  case type_SimpleArraySignedByte16:
#endif
#ifdef type_SimpleArraySignedByte30
	  case type_SimpleArraySignedByte30:
#endif
#ifdef type_SimpleArraySignedByte32
	  case type_SimpleArraySignedByte32:
#endif
	  case type_SimpleArraySingleFloat:
	  case type_SimpleArrayDoubleFloat:
#ifdef type_SimpleArrayLongFloat
	  case type_SimpleArrayLongFloat:
#endif
#ifdef type_SimpleArrayDoubleDoubleFloat
	  case type_SimpleArrayDoubleDoubleFloat:
#endif
#ifdef type_SimpleArrayComplexSingleFloat
	  case type_SimpleArrayComplexSingleFloat:
#endif
#ifdef type_SimpleArrayComplexDoubleFloat
	  case type_SimpleArrayComplexDoubleFloat:
#endif
#ifdef type_SimpleArrayComplexLongFloat
	  case type_SimpleArrayComplexLongFloat:
#endif
#ifdef type_SimpleArrayComplexDoubleDoubleFloat
	  case type_SimpleArrayComplexDoubleDoubleFloat:
#endif
	  case type_ComplexString:
	  case type_ComplexBitVector:
	  case type_ComplexVector:
	  case type_ComplexArray:
	      print_slots(array_slots, count, ptr);
	      break;

	  case type_CodeHeader:
	      print_slots(code_slots, count - 1, ptr);
	      break;

	  case type_FunctionHeader:
	  case type_ClosureFunctionHeader:
	      print_slots(fn_slots, 5, ptr);
	      break;

	  case type_ReturnPcHeader:
	      print_obj("code: ", obj - (count * 4));
	      break;

	  case type_ClosureHeader:
	      print_slots(closure_slots, count, ptr);
	      break;

	  case type_FuncallableInstanceHeader:
	      print_slots(funcallable_instance_slots, count, ptr);
	      break;

	  case type_ValueCellHeader:
	      print_slots(value_cell_slots, 1, ptr);
	      break;

	  case type_Sap:
	      NEWLINE;
#ifndef alpha
	      printf("0x%08lx", *ptr);
#else
	      printf("0x%016lx", *(long *) (ptr + 1));
#endif
	      break;

	  case type_WeakPointer:
	      print_slots(weak_pointer_slots, 3, ptr);
	      break;

	  case type_BaseChar:
	  case type_UnboundMarker:
	      NEWLINE;
	      printf("pointer to an immediate?");
	      break;

	  case type_Fdefn:
	      print_slots(fdefn_slots, count, ptr);
	      break;

#ifdef type_ScavengerHook
	  case type_ScavengerHook:
	      print_slots(scavenger_hook_slots, count, ptr);
	      break;
#endif

	  default:
	      NEWLINE;
	      printf("Unknown header object?");
	      break;
	}
    }
}

static void
print_obj(char *prefix, lispobj obj)
{
    static void (*verbose_fns[]) (lispobj obj)
	= { print_fixnum, print_otherptr, print_otherimm, print_list,
	print_fixnum, print_struct, print_otherimm, print_otherptr
    };
    static void (*brief_fns[]) (lispobj obj)
	= { brief_fixnum, brief_otherptr, brief_otherimm, brief_list,
	brief_fixnum, brief_struct, brief_otherimm, brief_otherptr
    };
    int type = LowtagOf(obj);
    struct var *var = lookup_by_obj(obj);
    char buffer[256];
    boolean verbose = cur_depth < brief_depth;


    if (!continue_p(verbose))
	return;

    if (var != NULL && var_clock(var) == cur_clock)
	dont_decend = TRUE;

    if (var == NULL
	&& (obj & type_FunctionPointer & type_ListPointer & type_InstancePointer
	    & type_OtherPointer) != 0)
	var = define_var(NULL, obj, FALSE);

    if (var != NULL)
	var_setclock(var, cur_clock);

    cur_depth++;
    if (verbose) {
	if (var != NULL) {
	    sprintf(buffer, "$%s=", var_name(var));
	    newline(buffer);
	} else
	    newline(NULL);
	printf("%s0x%08lx: ", prefix, obj);
	if (cur_depth < brief_depth) {
	    fputs(lowtag_Names[type], stdout);
	    (*verbose_fns[type]) (obj);
	} else
	    (*brief_fns[type]) (obj);
    } else {
	if (dont_decend)
	    printf("$%s", var_name(var));
	else {
	    if (var != NULL)
		printf("$%s=", var_name(var));
	    (*brief_fns[type]) (obj);
	}
    }
    cur_depth--;
    dont_decend = FALSE;
}

void
reset_printer()
{
    cur_clock++;
    cur_lines = 0;
    dont_decend = FALSE;
}

void
print(lispobj obj)
{
    skip_newline = TRUE;
    cur_depth = 0;
    max_depth = 5;
    max_lines = 20;

    print_obj("", obj);

    putchar('\n');
}

void
brief_print(lispobj obj)
{
    skip_newline = TRUE;
    cur_depth = 0;
    max_depth = 1;
    max_lines = 5000;

    print_obj("", obj);
    putchar('\n');
}
