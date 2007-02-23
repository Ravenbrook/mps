/*

 $Header: /project/cmucl/cvsroot/src/lisp/os-common.c,v 1.20 2005/09/15 18:26:52 rtoy Exp $

 This code was written as part of the CMU Common Lisp project at
 Carnegie Mellon University, and has been placed in the public domain.

*/

#include <stdio.h>
#include <string.h>

#include "os.h"
#include "internals.h"
#include "validate.h"
#include "lisp.h"
#include "lispregs.h"
#include "globals.h"
#include "interr.h"
#include "arch.h"
#include "interrupt.h"

/* Except for os_zero, these routines are only called by Lisp code.  These
   routines may also be replaced by os-dependent versions instead.  See
   hpux-os.c for some useful restrictions on actual usage. */

void
os_zero(addr, length)
     os_vm_address_t addr;
     os_vm_size_t length;
{
    os_vm_address_t block_start;
    os_vm_size_t block_size;

#ifdef PRINTNOISE
    fprintf(stderr, ";;; os_zero: addr: 0x%08x, len: 0x%08x\n", addr, length);
#endif

    block_start = os_round_up_to_page(addr);

    length -= block_start - addr;
    block_size = os_trunc_size_to_page(length);

    if (block_start > addr)
	memset((char *) addr, 0, block_start - addr);
    if (block_size < length)
	memset((char *) block_start + block_size, 0, length - block_size);

    if (block_size != 0) {
	/* Now deallocate and allocate the block so that it */
	/* faults in  zero-filled. */

	os_invalidate(block_start, block_size);
	addr = os_validate(block_start, block_size);

	if (addr == NULL || addr != block_start)
	    fprintf(stderr, "os_zero: block moved, 0x%p ==> 0x%8p!\n",
		    (void *) block_start, (void *) addr);
    }
}

os_vm_address_t
os_allocate(len)
     os_vm_size_t len;
{
    return os_validate((os_vm_address_t) NULL, len);
}

os_vm_address_t
os_allocate_at(os_vm_address_t addr, os_vm_size_t len)
{
    return os_validate(addr, len);
}

void
os_deallocate(addr, len)
     os_vm_address_t addr;
     os_vm_size_t len;
{
    os_invalidate(addr, len);
}

/* This function once tried to grow the chunk by asking os_validate if the
   space was available, but this really only works under Mach. */

os_vm_address_t
os_reallocate(os_vm_address_t addr, os_vm_size_t old_len, os_vm_size_t len)
{
    addr = os_trunc_to_page(addr);
    len = os_round_up_size_to_page(len);
    old_len = os_round_up_size_to_page(old_len);

    if (addr == NULL)
	return os_allocate(len);
    else {
	long len_diff = len - old_len;

	if (len_diff < 0)
	    os_invalidate(addr + len, -len_diff);
	else {
	    if (len_diff != 0) {
		os_vm_address_t new = os_allocate(len);

		if (new != NULL) {
		    memcpy((char *) new, (char *) addr, old_len);
		    os_invalidate(addr, old_len);
		}

		addr = new;
	    }
	}
	return addr;
    }
}

#ifdef LINKAGE_TABLE

/* These declarations are lies.  They actually take args, but are
   never called by C.  Only by Lisp */
extern void resolve_linkage_tramp(void);
extern void call_into_c(void);

/* In words */
#define LINKAGE_DATA_ENTRY_SIZE 3
#endif

void
os_foreign_linkage_init(void)
{
#ifdef LINKAGE_TABLE
    lispobj linkage_data_obj = SymbolValue(LINKAGE_TABLE_DATA);
    struct array *linkage_data = 0;
    long table_size = 0;
    struct vector *data_vector = 0;
    long i;

    linkage_data = (struct array *) PTR(linkage_data_obj);
    table_size = fixnum_value(linkage_data->fill_pointer);
    data_vector = (struct vector *) PTR(linkage_data->data);
    for (i = 0; i < table_size; i += LINKAGE_DATA_ENTRY_SIZE) {
	struct vector *symbol_name

	    = (struct vector *) PTR(data_vector->data[i]);
	long type = fixnum_value(data_vector->data[i + 1]);
	lispobj lib_list = data_vector->data[i + 2];

	/*
	 * Verify the "known" entries.  This had better match what
	 * init-foreign-linkage in new-genesis does!
	 */
	if (i == 0) {
#if defined(sparc)
	    if (type != 1 || strcmp((char *) symbol_name->data, "call_into_c")) {
		fprintf(stderr, "linkage_data is %s but expected call_into_c\n",
			(char *) symbol_name->data);
		lose("First element of linkage_data is bogus.\n");
	    }
	    arch_make_linkage_entry(i, &call_into_c, 1);
#elif defined(DARWIN)
	    if (type != 1 || strcmp((char *) symbol_name->data, "_call_into_c")) {
		fprintf(stderr, "linkage_data is %s but expected call_into_c\n",
			(char *) symbol_name->data);
		lose("First element of linkage_data is bogus.\n");
	    }
	    arch_make_linkage_entry(i, &call_into_c, 1);
#else
	    if (type != 1 || strcmp((char *) symbol_name->data,
				    "resolve_linkage_tramp")) {
		fprintf(stderr,
			"linkage_data is %s but expected resolve_linkage_tramp\n",
			(char *) symbol_name->data);
		lose("First element of linkage_data is bogus.\n");
	    }
	    arch_make_linkage_entry(i, &resolve_linkage_tramp, 1);
#endif
	    continue;
	}
	if (type == 2 && lib_list == NIL) {
	    void *target_addr = os_dlsym((char *) symbol_name->data, NIL);

	    if (!target_addr) {
		lose("%s is not defined.\n", (char *) symbol_name->data);
	    }
	    arch_make_linkage_entry(i / LINKAGE_DATA_ENTRY_SIZE, target_addr,
				    type);
	} else {
	    arch_make_lazy_linkage(i / LINKAGE_DATA_ENTRY_SIZE);
	}

    }
#endif /* LINKAGE_TABLE */
}

/* At the second stage of initialization, after Lisp has dlopened all
   needed shared libraries, go back through the table and initialize
   data symbols. */

void
os_resolve_data_linkage(void)
{
#ifdef LINKAGE_TABLE
    lispobj linkage_data_obj = SymbolValue(LINKAGE_TABLE_DATA);
    struct array *linkage_data = 0;
    long table_size = 0;
    struct vector *data_vector = 0;
    long i;

    linkage_data = (struct array *) PTR(linkage_data_obj);
    table_size = fixnum_value(linkage_data->fill_pointer);
    data_vector = (struct vector *) PTR(linkage_data->data);
    for (i = 0; i < table_size; i += LINKAGE_DATA_ENTRY_SIZE) {
	struct vector *symbol_name

	    = (struct vector *) PTR(data_vector->data[i]);
	long type = fixnum_value(data_vector->data[i + 1]);
	lispobj lib_list = data_vector->data[i + 2];

	if (type == 2 && lib_list != NIL) {
	    void *target_addr = os_dlsym((char *) symbol_name->data, lib_list);

	    if (!target_addr) {
		lose("%s is not defined.\n", (char *) symbol_name->data);
	    }
	    arch_make_linkage_entry(i / LINKAGE_DATA_ENTRY_SIZE, target_addr,
				    type);
	}
    }
#endif /* LINKAGE_TABLE */
}

/* Make entry for the symbol at entry in LINKAGE_TABLE_DATA.  Called
   from register-foreign-linkage. */
#ifdef LINKAGE_TABLE
extern void undefined_foreign_symbol_trap(lispobj arg);
#endif

unsigned long
os_link_one_symbol(long entry)
{
#ifdef LINKAGE_TABLE
    lispobj linkage_data_obj = SymbolValue(LINKAGE_TABLE_DATA);
    struct array *linkage_data = 0;
    long table_size = 0;
    struct vector *data_vector = 0;
    struct vector *symbol_name;
    long type;
    void *target_addr;
    long table_index = entry * LINKAGE_DATA_ENTRY_SIZE;

    linkage_data = (struct array *) PTR(linkage_data_obj);
    table_size = fixnum_value(linkage_data->fill_pointer);
    if (table_index >= table_size - 1) {
	return 0;
    }
    data_vector = (struct vector *) PTR(linkage_data->data);
    symbol_name = (struct vector *) PTR(data_vector->data[table_index]);
    type = fixnum_value(data_vector->data[table_index + 1]);
    target_addr = os_dlsym((char *) symbol_name->data,
			   data_vector->data[table_index + 2]);
#if 0
    fprintf(stderr, "Looked up %s symbol %s at %lx\n",
	    type == 1 ? "code" : "data",
	    (char *) symbol_name->data, (unsigned long) target_addr);
#endif
    if (!target_addr) {
	undefined_foreign_symbol_trap((lispobj) data_vector->data[table_index]);
    }
    arch_make_linkage_entry(entry, target_addr, type);
    return (unsigned long) target_addr;
#else
    return 0;
#endif /* LINKAGE_TABLE */
}

unsigned long
lazy_resolve_linkage(unsigned long retaddr)
{
#ifdef LINKAGE_TABLE
    unsigned long target_addr = os_link_one_symbol(arch_linkage_entry(retaddr));

    return target_addr;
#else
    return 0;
#endif /* LINKAGE_TABLE */
}

static int
os_stack_grows_down_1(int *local_var_address)
{
    int dummy;

    return &dummy < local_var_address;
}

/* Value is true if the processor stack grows down.  */

int
os_stack_grows_down(void)
{
    int dummy;

    return os_stack_grows_down_1(&dummy);
}


#ifdef RED_ZONE_HIT

/* The end of the control stack contains two guard zones:

   +----------+ stack start (stack growing down)
   |          |
       ...
   |          |
   +----------+
   |          | yellow zone
   +----------+
   |          | red zone
   +----------+				CONTROL_STACK_START

   Both the yellow zone and the red zone are write-protected.

   When entering the yellow zone, we unprotect the yellow zone and
   make Lisp signal a control stack exhausted error, with stack
   contents left intact for the debugger, which is entered.

   When hitting the red zone we arrange for calling a function that
   throws back to the top-level.  */

#ifndef YELLOW_ZONE_SIZE
#define YELLOW_ZONE_SIZE 0x8000	/* 32K */
#endif

#ifndef RED_ZONE_SIZE
#define RED_ZONE_SIZE YELLOW_ZONE_SIZE
#endif

/* Return the start addresses of the yellow and red zones in
   *YELLOW_START and *RED_START.  */

static void
guard_zones(char **yellow_start, char **red_start)
{
#if (defined(i386) || defined(__x86_64))
    if (os_stack_grows_down()) {
	char *end = (char *) CONTROL_STACK_START;

	*red_start = end;
	*yellow_start = *red_start + RED_ZONE_SIZE;
    } else {
	char *end = (char *) CONTROL_STACK_START + CONTROL_STACK_SIZE;

	*red_start = end - RED_ZONE_SIZE;
	*yellow_start = *red_start - YELLOW_ZONE_SIZE;
    }
#else
    /*
     * On Solaris/sparc, the C stack grows down, but the Lisp control
     * stack grows up.  The stack zones begin just before the end of the
     * control stack area.
     */

    char *end = (char *) CONTROL_STACK_START + CONTROL_STACK_SIZE;

    *red_start = end - RED_ZONE_SIZE;
    *yellow_start = *red_start - YELLOW_ZONE_SIZE;
#endif
}

/* Return the guard zone FAULT_ADDR is in or 0 if not in a guard
   zone.  */

static int
control_stack_zone(void *fault_addr)
{
    char *yellow_start, *red_start;
    char *p = (char *) fault_addr;

    guard_zones(&yellow_start, &red_start);

    if (p >= yellow_start && p < yellow_start + YELLOW_ZONE_SIZE)
	return YELLOW_ZONE;
    else if (p >= red_start && p < red_start + RED_ZONE_SIZE)
	return RED_ZONE;
    else
	return 0;
}

/* Protect/unprotect the guard zone ZONE of the control stack.  */

void
os_guard_control_stack(int zone, int guard)
{
    char *yellow_start, *red_start;
    int flags;

    guard_zones(&yellow_start, &red_start);

    if (guard)
	flags = OS_VM_PROT_READ | OS_VM_PROT_EXECUTE;
    else
	flags = OS_VM_PROT_ALL;

    if (zone == YELLOW_ZONE)
	os_protect((os_vm_address_t) yellow_start, YELLOW_ZONE_SIZE, flags);
    else if (zone == RED_ZONE)
	os_protect((os_vm_address_t) red_start, RED_ZONE_SIZE, flags);
    else {
	char *start = red_start < yellow_start ? red_start : yellow_start;

	os_protect((os_vm_address_t) start, RED_ZONE_SIZE + YELLOW_ZONE_SIZE,
		   flags);
    }
}

/* Handle a possible guard zone hit at FAULT_ADDR.  Value is
   non-zero if FAULT_ADDR is in a guard zone.  */

int
os_control_stack_overflow(void *fault_addr, os_context_t * context)
{
    enum stack_zone_t zone;

    zone = control_stack_zone(fault_addr);

    if (zone == YELLOW_ZONE || zone == RED_ZONE) {
	lispobj error;

#if 0
	fprintf(stderr, "hit end of control stack in zone %s\n",
		(zone == YELLOW_ZONE) ? "YELLOW" : (zone ==
						    RED_ZONE) ? "RED" : "BOTH");
#endif
	/* Unprotect the stack, giving us some room on the stack for
	   error handling in Lisp.  Fake a stack frame for this
	   interruption.  */
	os_guard_control_stack(zone, 0);

	build_fake_control_stack_frame(context);

	/* The protection violation signal is delivered on a signal
	   stack different from the normal stack, so that we don't
	   trample on the guard pages of the normal stack while handling
	   the signal.  To get a Lisp function called when the signal
	   handler returns, we change the return address of the signal
	   context to the address of the function we want to be
	   called.  */
	if (zone == RED_ZONE)
	    error = SymbolFunction(RED_ZONE_HIT);
	else
	    error = SymbolFunction(YELLOW_ZONE_HIT);

#ifdef i386
	/* ECX is the argument count.  */
	SC_PC(context) = (int) ((struct function *) PTR(error))->code;
#if USE_SA_SIGINFO
	context->uc_mcontext.__gregs[_REG_ECX] == 0;
#else
	context->sc_ecx = 0;
#endif
#else
#ifdef __x86_64
	/* RCX is the argument count.  */
	context->sc_rip =
	    (unsigned long) ((struct function *) PTR(error))->code;
	context->sc_rcx = 0;
#else
#ifdef sparc
	/* This part should be common to all non-x86 ports */
	SC_PC(context) = (long) ((struct function *) PTR(error))->code;
	SC_NPC(context) = SC_PC(context) + 4;
	SC_REG(context, reg_NARGS) = 0;
	SC_REG(context, reg_LIP) =
	    (long) ((struct function *) PTR(error))->code;
	SC_REG(context, reg_CFP) = (long) current_control_frame_pointer;
	/* This is sparc specific */
	SC_REG(context, reg_CODE) = ((long) PTR(error)) + type_FunctionPointer;
#else
#error os_control_stack_overflow not implemented for this system
#endif
#endif
#endif
	return 1;
    }

    return 0;
}

#else /* not RED_ZONE_HIT */

/* Dummy for bootstrapping.  */

void
os_guard_control_stack(int zone, int guard)
{
}

#endif /* not RED_ZONE_HIT */
