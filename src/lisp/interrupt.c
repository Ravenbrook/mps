/* $Header: /project/cmucl/cvsroot/src/lisp/interrupt.c,v 1.43 2005/09/15 18:26:51 rtoy Exp $ */

/* Interrupt handling magic. */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <signal.h>
#include <assert.h>

#include "lisp.h"
#include "arch.h"
#include "internals.h"
#include "os.h"
#include "interrupt.h"
#include "globals.h"
#include "lispregs.h"
#include "validate.h"
#include "monitor.h"
#include "gc.h"
#include "alloc.h"
#include "dynbind.h"
#include "interr.h"

boolean internal_errors_enabled = 0;

os_context_t *lisp_interrupt_contexts[MAX_INTERRUPTS];

union interrupt_handler interrupt_handlers[NSIG];
void (*interrupt_low_level_handlers[NSIG]) (HANDLER_ARGS) = {
0};

static int pending_signal = 0;

#if defined(SOLARIS) || defined(__OpenBSD__) || defined(__NetBSD__)
static siginfo_t *pending_code;

#define PASSCODE(code) ((code))
#define DEREFCODE(code) ((code))
#else
static int pending_code = 0;

#define PASSCODE(code) (code)
#define DEREFCODE(code) (code)
#endif

#ifdef POSIX_SIGS
static sigset_t pending_mask;
#else
static int pending_mask = 0;
#endif
static boolean maybe_gc_pending = FALSE;


/****************************************************************\
* Utility routines used by various signal handlers.              *
\****************************************************************/

void
build_fake_control_stack_frame(os_context_t * context)
{
#if !(defined(i386) || defined(__x86_64))
    lispobj oldcont;

    /* Build a fake stack frame */
    current_control_frame_pointer = (lispobj *) SC_REG(context, reg_CSP);
    if ((lispobj *) SC_REG(context, reg_CFP) == current_control_frame_pointer) {
	/* There is a small window during call where the callee's frame */
	/* isn't built yet. */
	if (LowtagOf(SC_REG(context, reg_CODE)) == type_FunctionPointer) {
	    /* We have called, but not built the new frame, so
	       build it for them. */
	    current_control_frame_pointer[0] = SC_REG(context, reg_OCFP);
	    current_control_frame_pointer[1] = SC_REG(context, reg_LRA);
	    current_control_frame_pointer += 8;
	    /* Build our frame on top of it. */
	    oldcont = (lispobj) SC_REG(context, reg_CFP);
	} else {
	    /* We haven't yet called, build our frame as if the
	       partial frame wasn't there. */
	    oldcont = (lispobj) SC_REG(context, reg_OCFP);
	}
    }
    /* ### We can't tell if we are still in the caller if it had to
       reg_ALLOCate the stack frame due to stack arguments. */
    /* ### Can anything strange happen during return? */
    else {

	/* Normal case. */
	oldcont = (lispobj) SC_REG(context, reg_CFP);
    }

    current_control_stack_pointer = current_control_frame_pointer + 8;

    current_control_frame_pointer[0] = oldcont;
    current_control_frame_pointer[1] = NIL;
    current_control_frame_pointer[2] = (lispobj) SC_REG(context, reg_CODE);
#endif
}

void
fake_foreign_function_call(os_context_t * context)
{
    int context_index;

    /* Get current LISP state from context */
#ifdef reg_ALLOC
    current_dynamic_space_free_pointer = (lispobj *) SC_REG(context, reg_ALLOC);
#ifdef alpha
    if ((long) current_dynamic_space_free_pointer & 1) {
	printf("Dead in fake_foriegn_function-call, context = %x\n", context);
	lose("");
    }
#endif
#endif
#ifdef reg_BSP
    current_binding_stack_pointer = (lispobj *) SC_REG(context, reg_BSP);
#endif

    build_fake_control_stack_frame(context);

    /* Do dynamic binding of the active interrupt context index
       and save the context in the context array. */
    context_index = SymbolValue(FREE_INTERRUPT_CONTEXT_INDEX) >> 2;

    if (context_index >= MAX_INTERRUPTS) {
	fprintf(stderr,
		"Maximum number (%d) of interrupts exceeded.  Exiting.\n",
		MAX_INTERRUPTS);
	exit(1);
    }

    bind_variable(FREE_INTERRUPT_CONTEXT_INDEX, make_fixnum(context_index + 1));

    lisp_interrupt_contexts[context_index] = context;

    /* No longer in Lisp now. */
    foreign_function_call_active = 1;
}

void
undo_fake_foreign_function_call(os_context_t * context)
{
    /* Block all blockable signals */
#ifdef POSIX_SIGS
    sigset_t block;

    sigemptyset(&block);
    FILLBLOCKSET(&block);
    sigprocmask(SIG_BLOCK, &block, 0);
#else
    sigblock(BLOCKABLE);
#endif

    /* Going back into lisp. */
    foreign_function_call_active = 0;

    /* Undo dynamic binding. */
    /* ### Do I really need to unbind_to_here()? */
    unbind();

#ifdef reg_ALLOC
    /* Put the dynamic space free pointer back into the context. */
    SC_REG(context, reg_ALLOC) =
	(unsigned long) current_dynamic_space_free_pointer;
#endif
}

void
interrupt_internal_error(HANDLER_ARGS, boolean continuable)
{
    lispobj context_sap = NIL;

#if ( defined( __linux__ ) && (defined( i386 ) || defined( __x86_64 ) ) )
    GET_CONTEXT
#endif
	fake_foreign_function_call(context);

    /* Allocate the SAP object while the interrupts are still disabled. */
    if (internal_errors_enabled)
	context_sap = alloc_sap(context);

#ifdef POSIX_SIGS
#if !defined(__linux__) || (defined(__linux__) && (__GNU_LIBRARY__ < 6))
    sigprocmask(SIG_SETMASK, &context->uc_sigmask, 0);
#else
    {
	sigset_t temp;

	sigemptyset(&temp);
	temp.__val[0] = context->uc_sigmask;
	sigprocmask(SIG_SETMASK, &temp, 0);
    }
#endif
#else
    sigsetmask(context->sc_mask);
#endif /* POSIX_SIGS */

    if (internal_errors_enabled)
	funcall2(SymbolFunction(INTERNAL_ERROR), context_sap,
		 continuable ? T : NIL);
    else
	internal_error(context);
    undo_fake_foreign_function_call(context);
    if (continuable)
	arch_skip_instruction(context);
}

void
interrupt_handle_pending(os_context_t * context)
{
    boolean were_in_lisp = !foreign_function_call_active;

    SetSymbolValue(INTERRUPT_PENDING, NIL);

    if (maybe_gc_pending) {
	maybe_gc_pending = FALSE;
#ifndef i386
	if (were_in_lisp)
#endif
	    fake_foreign_function_call(context);
	funcall0(SymbolFunction(MAYBE_GC));
#ifndef i386
	if (were_in_lisp)
#endif
	    undo_fake_foreign_function_call(context);
    }
#ifdef POSIX_SIGS
#if  !defined(__linux__) || (defined(__linux__) && (__GNU_LIBRARY__ < 6))
    context->uc_sigmask = pending_mask;
#else
    context->uc_sigmask = pending_mask.__val[0];
#endif
    sigemptyset(&pending_mask);
#else
    context->sc_mask = pending_mask;
    pending_mask = 0;
#endif /* POSIX_SIGS */

    if (pending_signal) {
	int signal;

#if defined(SOLARIS) || defined(__OpenBSD__) || defined(__NetBSD__)
	siginfo_t *code;
#else
	int code;
#endif
	signal = pending_signal;
	code = pending_code;
	pending_signal = 0;
	/* pending_code = 0; */
#if ( defined( __linux__ ) && ( defined( i386 ) || defined ( __x86_64 ) ) )
	interrupt_handle_now(signal, *context);
#else
	interrupt_handle_now(signal, PASSCODE(code), context);
#endif
    }
}


/****************************************************************\
* interrupt_handle_now_handler, maybe_now_maybe_later            *
*    the two main signal handlers.                               *
* interrupt_handle_now                                           *
*    is called from those to do the real work, but isn't itself  *
*    a handler.                                                  *
\****************************************************************/

void
interrupt_handle_now_handler(HANDLER_ARGS)
{
#if defined(__linux__) && (defined(i386) || defined(__x86_64))
    interrupt_handle_now(signal, contextstruct);
#else
    interrupt_handle_now(signal, code, context);
#endif

#ifdef DARWIN
    /* Work around G5 bug; fix courtesy gbyers via chandler */
    sigreturn(context);
#endif
}

void
interrupt_handle_now(HANDLER_ARGS)
{
#if defined(__linux__) && (defined(i386) || defined(__x86_64))
    GET_CONTEXT
#endif
    int were_in_lisp;
    union interrupt_handler handler;

#if defined(__linux__) && ( defined(i386) || defined(__x86_64) )
    /*
     * Restore the FPU control word, setting the rounding mode to nearest.
     */

    if (contextstruct.fpstate)
#if defined(__x86_64)
	setfpucw(contextstruct.fpstate->cwd & ~0xc00);
#else
	setfpucw(contextstruct.fpstate->cw & ~0xc00);
#endif
#endif

    handler = interrupt_handlers[signal];

    if (handler.c == (void (*)(HANDLER_ARGS)) SIG_IGN)
	return;

    SAVE_CONTEXT();
    /**/ were_in_lisp = !foreign_function_call_active;
#if ! (defined(i386) || defined(_x86_64))
    if (were_in_lisp)
#endif
	fake_foreign_function_call(context);

    if (handler.c == (void (*)(HANDLER_ARGS)) SIG_DFL)
	/* This can happen if someone tries to ignore or default on one of the */
	/* signals we need for runtime support, and the runtime support */
	/* decides to pass on it.  */
	lose("interrupt_handle_now: No handler for signal %d?\n", signal);
    else if (LowtagOf(handler.lisp) == type_FunctionPointer) {
	/* Allocate the SAP object while the interrupts are still
	   disabled. */
	lispobj context_sap = alloc_sap(context);

	/* Allow signals again. */
#ifdef POSIX_SIGS
#if  !defined(__linux__) || (defined(__linux__) && (__GNU_LIBRARY__ < 6))
	sigprocmask(SIG_SETMASK, &context->uc_sigmask, 0);
#else
	{
	    sigset_t temp;

	    sigemptyset(&temp);
	    temp.__val[0] = context->uc_sigmask;
	    sigprocmask(SIG_SETMASK, &temp, 0);
	}
#endif
#else
	sigsetmask(context->sc_mask);
#endif /* POSIX_SIGS */

#if 1
	funcall3(handler.lisp, make_fixnum(signal), make_fixnum(CODE(code)),
		 context_sap);
#else
	funcall3(handler.lisp, make_fixnum(signal), alloc_sap(code),
		 alloc_sap(context));
#endif
    } else {
	/* Allow signals again. */
#ifdef POSIX_SIGS
#if !defined(__linux__) || (defined(__linux__) && (__GNU_LIBRARY__ < 6))
	sigprocmask(SIG_SETMASK, &context->uc_sigmask, 0);
#else
	{
	    sigset_t temp;

	    sigemptyset(&temp);
	    temp.__val[0] = context->uc_sigmask;
	    sigprocmask(SIG_SETMASK, &temp, 0);
	}
#endif
#else
	sigsetmask(context->sc_mask);
#endif /* POSIX_SIGS */

#if ( defined( __linux__ ) && ( defined( i386 ) || defined ( __x86_64 ) ) )
	(*handler.c) (signal, contextstruct);
#else
	(*handler.c) (signal, code, context);
#endif
    }

#if !(defined(i386) || defined(__x86_64))
    if (were_in_lisp)
#endif
	undo_fake_foreign_function_call(context);
}

static void
maybe_now_maybe_later(HANDLER_ARGS)
{
#if defined(__linux__) && (defined(i386) || defined(__x86_64))
    GET_CONTEXT
#endif
	SAVE_CONTEXT();
    /**/ if (SymbolValue(INTERRUPTS_ENABLED) == NIL) {
	pending_signal = signal;
	pending_code = DEREFCODE(code);
#ifdef POSIX_SIGS
#if !defined(__linux__) || (defined(__linux__) && (__GNU_LIBRARY__ < 6))
	pending_mask = context->uc_sigmask;
	FILLBLOCKSET(&context->uc_sigmask);
#else
	{
	    sigset_t temp;

	    sigemptyset(&temp);
	    pending_mask.__val[0] = context->uc_sigmask;
	    temp.__val[0] = context->uc_sigmask;
	    FILLBLOCKSET(&temp);

	    context->uc_sigmask = temp.__val[0];
	}
#endif

#else
	pending_mask = context->sc_mask;
	context->sc_mask |= BLOCKABLE;
#endif /* POSIX_SIGS */

	SetSymbolValue(INTERRUPT_PENDING, T);
    } else if (
#if !(defined(i386) || defined(__x86_64))
		  (!foreign_function_call_active) &&
#endif
		  arch_pseudo_atomic_atomic(context)) {
	pending_signal = signal;
	pending_code = DEREFCODE(code);
#ifdef POSIX_SIGS
#if !defined(__linux__) || (defined(__linux__) && (__GNU_LIBRARY__ < 6))
	pending_mask = context->uc_sigmask;
	FILLBLOCKSET(&context->uc_sigmask);
#else
	{
	    sigset_t temp;

	    sigemptyset(&temp);
	    pending_mask.__val[0] = context->uc_sigmask;
	    temp.__val[0] = context->uc_sigmask;
	    FILLBLOCKSET(&temp);
	    context->uc_sigmask = temp.__val[0];
	}
#endif
#else
	pending_mask = context->sc_mask;
	context->sc_mask |= BLOCKABLE;
#endif /* POSIX_SIGS */

	arch_set_pseudo_atomic_interrupted(context);
    } else {
#if defined(__linux__) && (defined(i386) || defined(__x86_64))
	/*
	 * Restore the FPU control word, setting the rounding mode to nearest.
	 */

	if (contextstruct.fpstate)
#if defined(__x86_64)
	    setfpucw(contextstruct.fpstate->cwd & ~0xc00);
#else
	    setfpucw(contextstruct.fpstate->cw & ~0xc00);
#endif
#endif

#if ( defined( __linux__ ) && ( defined( i386 ) || defined( __x86_64 ) ) )
	interrupt_handle_now(signal, contextstruct);
#else
	interrupt_handle_now(signal, code, context);
#endif

    }

#ifdef DARWIN
    /* Work around G5 bug; fix courtesy gbyers via chandler */
    sigreturn(context);
#endif
}

/****************************************************************\
* Stuff to detect and handle hitting the gc trigger.             *
\****************************************************************/

#ifndef INTERNAL_GC_TRIGGER
static boolean
gc_trigger_hit(HANDLER_ARGS)
{
    if (current_auto_gc_trigger == NULL) {
	return FALSE;
    } else {
	lispobj *badaddr = (lispobj *) arch_get_bad_addr(signal, code, context);

#ifdef PRINTNOISE
	fprintf(stderr,
		"gc_trigger_hit: badaddr=%p, current_auto_gc_trigger=%p, limit=%p\n",
		badaddr, current_auto_gc_trigger,
		current_dynamic_space + dynamic_space_size);
#endif
	return (badaddr >= current_auto_gc_trigger &&
		(unsigned long) badaddr <
		(unsigned long) current_dynamic_space +
		(unsigned long) dynamic_space_size);
    }
}
#endif

#if !(defined(i386) || defined(__x86_64))
boolean
interrupt_maybe_gc(HANDLER_ARGS)
{
    if (!foreign_function_call_active
#ifndef INTERNAL_GC_TRIGGER
	&& gc_trigger_hit(signal, code, context)
#endif
	) {
#ifndef INTERNAL_GC_TRIGGER
	clear_auto_gc_trigger();
#endif

	if (arch_pseudo_atomic_atomic(context)) {
	    maybe_gc_pending = TRUE;
	    if (pending_signal == 0) {
#ifdef POSIX_SIGS
#if !defined(__linux__) || (defined(__linux__) && (__GNU_LIBRARY__ < 6))
		pending_mask = context->uc_sigmask;
		FILLBLOCKSET(&context->uc_sigmask);
#else
		{
		    sigset_t temp;

		    sigemptyset(&temp);
		    pending_mask.__val[0] = context->uc_sigmask;
		    temp.__val[0] = context->uc_sigmask;
		    FILLBLOCKSET(&temp);

		    context->uc_sigmask = temp.__val[0];
		}
#endif
#else
		pending_mask = context->sc_mask;
		context->sc_mask |= BLOCKABLE;
#endif /* POSIX_SIGS */
	    }
	    arch_set_pseudo_atomic_interrupted(context);
	} else {
	    fake_foreign_function_call(context);
	    funcall0(SymbolFunction(MAYBE_GC));
	    undo_fake_foreign_function_call(context);
	}

	return TRUE;
    } else
	return FALSE;
}
#endif

/****************************************************************\
* Noise to install handlers.                                     *
\****************************************************************/

#if !(defined(i386) || defined(__x86_64))
#define SIGNAL_STACK_SIZE SIGSTKSZ
static char altstack[SIGNAL_STACK_SIZE];
#endif

void
interrupt_install_low_level_handler(int signal, void handler(HANDLER_ARGS))
{
#ifdef POSIX_SIGS
    struct sigaction sa;

    sa.sa_sigaction = handler;
    sigemptyset(&sa.sa_mask);
    FILLBLOCKSET(&sa.sa_mask);
    sa.sa_flags = SA_RESTART | USE_SA_SIGINFO;

    /* Deliver protection violations on a dedicated signal stack,
       because, when we get that signal because of hitting a control
       stack guard zone, it's not a good idea to use more of the
       control stack for handling the signal.  */
    /* But we only need this on x86 since the Lisp control stack and the
       C control stack are the same.  For others, they're separate so
       the C stack can still be used.  */
#ifdef RED_ZONE_HIT
    if (signal == PROTECTION_VIOLATION_SIGNAL) {
	stack_t sigstack;

#if (defined( i386 ) || defined(__x86_64))
	sigstack.ss_sp = (void *) SIGNAL_STACK_START;
#else
	sigstack.ss_sp = (void *) altstack;
#endif
	sigstack.ss_flags = 0;
	sigstack.ss_size = SIGNAL_STACK_SIZE;
	if (sigaltstack(&sigstack, 0) == -1)
	    perror("sigaltstack");
	sa.sa_flags |= SA_ONSTACK;
    }
#endif /* RED_ZONE_HIT */

    sigaction(signal, &sa, NULL);

#else /* not POSIX_SIGNALS */
    struct sigvec sv;

    sv.sv_handler = handler;
    sv.sv_mask = BLOCKABLE;
    sv.sv_flags = 0;
    sigvec(signal, &sv, NULL);
#endif /* not POSIX_SIGNALS */

    if (handler == (void (*)(HANDLER_ARGS)) SIG_DFL)
	interrupt_low_level_handlers[signal] = 0;
    else
	interrupt_low_level_handlers[signal] = handler;
}

unsigned long
install_handler(int signal, void handler(HANDLER_ARGS))
#ifdef POSIX_SIGS
{
    struct sigaction sa;
    sigset_t old, new;
    union interrupt_handler oldhandler;

    sigemptyset(&new);
    sigaddset(&new, signal);
    sigprocmask(SIG_BLOCK, &new, &old);

    sigemptyset(&new);
    FILLBLOCKSET(&new);

    if (interrupt_low_level_handlers[signal] == 0) {
	if (handler == (void (*)(HANDLER_ARGS)) SIG_DFL
	    || handler == (void (*)(HANDLER_ARGS)) SIG_IGN)
	    sa.sa_sigaction = handler;
	else if (sigismember(&new, signal))
	    sa.sa_sigaction = maybe_now_maybe_later;
	else
	    sa.sa_sigaction = interrupt_handle_now_handler;

	sigemptyset(&sa.sa_mask);
	FILLBLOCKSET(&sa.sa_mask);
	sa.sa_flags = USE_SA_SIGINFO | SA_RESTART;

	sigaction(signal, &sa, NULL);
    }

    oldhandler = interrupt_handlers[signal];
    interrupt_handlers[signal].c = handler;

    sigprocmask(SIG_SETMASK, &old, 0);

    return (unsigned long) oldhandler.lisp;
}
#else
{
    struct sigvec sv;
    int oldmask;
    union interrupt_handler oldhandler;

    oldmask = sigblock(sigmask(signal));

    if (interrupt_low_level_handlers[signal] == 0) {
	if (handler == (void (*)(HANDLER_ARGS)) SIG_DFL
	    || handler == (void (*)(HANDLER_ARGS)) SIG_IGN)
	    sv.sv_handler = handler;
	else if (sigmask(signal) & BLOCKABLE)
	    sv.sv_handler = maybe_now_maybe_later;
	else
	    sv.sv_handler = interrupt_handle_now_handler;

	sv.sv_mask = BLOCKABLE;
	sv.sv_flags = 0;
	sigvec(signal, &sv, NULL);
    }

    oldhandler = interrupt_handlers[signal];
    interrupt_handlers[signal].c = handler;

    sigsetmask(oldmask);

    return (unsigned long) oldhandler.lisp;
}
#endif

#ifdef FEATURE_HEAP_OVERFLOW_CHECK
void
interrupt_handle_space_overflow(lispobj error, os_context_t * context)
{
#ifdef i386
    /* ECX is the argument count.  */
#if USE_SA_SIGINFO
    context->uc_mcontext.__gregs[_REG_EIP] ==
	(int) ((struct function *) PTR(error))->code;
    context->uc_mcontext.__gregs[_REG_ECX] == 0;
#else
    SC_PC(context) = (int) ((struct function *) PTR(error))->code;
    context->sc_ecx = 0;
#endif
#else
#ifdef __x86_64
    /* RCX is the argument count.  */
    context->sc_rip = (unsigned long) ((struct function *) PTR(error))->code;
    context->sc_rcx = 0;
#else
#ifdef sparc
    build_fake_control_stack_frame(context);
    /* This part should be common to all non-x86 ports */
    SC_PC(context) = (long) ((struct function *) PTR(error))->code;
    SC_NPC(context) = SC_PC(context) + 4;
    SC_REG(context, reg_NARGS) = 0;
    SC_REG(context, reg_LIP) = (long) ((struct function *) PTR(error))->code;
    SC_REG(context, reg_CFP) = (long) current_control_frame_pointer;
    /* This is sparc specific */
    SC_REG(context, reg_CODE) = ((long) PTR(error)) + type_FunctionPointer;
#else
#error interrupt_handle_space_overflow not implemented for this system
#endif
#endif
#endif
}
#endif /* FEATURE_HEAP_OVERFLOW_CHECK */

void
interrupt_init(void)
{
    int i;

    for (i = 0; i < NSIG; i++)
	interrupt_handlers[i].c = (void (*)(HANDLER_ARGS)) SIG_DFL;
}
