/* scport.c -- port operations
 *
 * Richard Brooksby, 2004-08-04
 *
 * $Id$
 *
 * 2004-08-04  RB  Split off from sc.c code written since 1996-08-30.
 */

#include "sc.h"

#include <stdio.h>


/* current-input-port
 * current-output-port
 * current-error-port @@@@ non-standard
 *
 * "Returns the current default input or output port." -- R5RS
 */

static void current_in_entry(state_t state)
{
  check_args(state, 0);
  RET1(state->inport);
}

static void current_out_entry(state_t state)
{
  check_args(state, 0);
  RET1(state->outport);
}

static void current_err_entry(state_t state)
{
  check_args(state, 0);
  RET1(state->errport);
}


/* set-current-input-port! port
 * set-current-output-port! port
 * set-current-error-port! port @@@@ non-standard
 *
 * Non-standard procedures used to implement "with-current-(input|output)-port".
 */

static void set_current_in_entry(state_t state)
{
  check_args(state, 1, TYPE_INPORT);
  state->inport = A0;
  RET0();
}

static void set_current_out_entry(state_t state)
{
  check_args(state, 1, TYPE_OUTPORT);
  state->outport = A0;
  RET0();
}

static void set_current_err_entry(state_t state)
{
  check_args(state, 1, TYPE_OUTPORT);
  state->errport = A0;
  RET0();
}


/* open-input-file filename
 *
 * "Takes a string naming an existing file and returns an input port
 *  capable of delivering characters from the file. If the file cannot
 *  be opened, an error is signalled." -- R5RS
 *
 * The results are undefined if the filename contains a NUL.
 */

static void open_in_entry(state_t state)
{
  FILE *stream;
  check_args(state, 1, TYPE_STRING);
  stream = fopen(STR(A0), "r");
  if(stream == NULL)
    error(state, "cannot open input file \"%s\"", STR(A0));
  MAKE_INPORT(T0, stream);
  RET1(T0);
}


/* open-output-file filename
 *
 * "Takes a string naming an output file to be created and returns an
 *  output port capable of writing characters to a new file by that
 *  name. If the file cannot be opened, an error is signalled. If a
 *  file with the given name already exists, the effect is
 *  unspecified." -- R5RS
 *
 * The results are undefined if the filename contains a NUL.
 */

static void open_out_entry(state_t state)
{
  FILE *stream;
  check_args(state, 1, TYPE_STRING);
  stream = fopen(STR(A0), "w");
  if(stream == NULL)
    error(state, "cannot open output file \"%s\"", STR(A0));
  MAKE_OUTPORT(T0, stream);
  RET1(T0);
}


/* close-input-port
 * close-output-port
 *
 * "Closes the file associated with port, rendering the port incapable
 *  of delivering or accepting characters.  These routines
 *  [close-input-port and close-output-port] have no effect if the file
 *  has already been closed. The value returned is unspecified." -- R5RS
 */

static void close_in_entry(state_t state)
{
  check_args(state, 1, TYPE_INPORT);
  if(INPORTSTREAM(A0) == NULL)
    error(state, "port already closed");
  (void)fclose(INPORTSTREAM(A0));
  INPORTSTREAM(A0) = NULL;
  RET0();
}

static void close_out_entry(state_t state)
{
  check_args(state, 1, TYPE_OUTPORT);
  if(OUTPORTSTREAM(A0) == NULL)
    error(state, "port already closed");
  (void)fclose(OUTPORTSTREAM(A0));
  OUTPORTSTREAM(A0) = NULL;
  RET0();
}


/* eof-object? obj 
 *  
 * "Returns #t if obj is an end of file object, otherwise returns #f.
 *  The precise set of end of file objects will vary among
 *  implementations, but in any case no end of file object will ever
 *  be an object that can be read in using `read'." -- R5RS
 */

static void eof_objectp_entry(state_t state)
{
  check_args(state, 1, 0);
  T0 = A0 == obj_eof ? obj_true : obj_false;
  RET1(T0);
}


/* proctab -- initial procedure table
 *
 * The procedures listed in this table are initialized and added
 * to the environment by "unit_entry".
 */

static const proctab_s proctab[] = {
  {"open-input-file",		open_in_entry},
  {"open-output-file",		open_out_entry},
  {"close-input-port",		close_in_entry},
  {"close-output-port",		close_out_entry},
  {"current-input-port",	current_in_entry},
  {"current-output-port",	current_out_entry},
  {"current-error-port",	current_err_entry},
  {"set-current-input-port!",	set_current_in_entry},
  {"set-current-output-port!",	set_current_out_entry},
  {"set-current-error-port!",	set_current_err_entry},
  {"eof-object?",		eof_objectp_entry},
  {NULL,                        NULL}
};


/* syntab -- syntax table
 *
 * This table used by "unit_entry" to create the initial syntax environment.
 */

static const proctab_s syntab[] = {
  {NULL, NULL}
};


static void unit_entry(state_t state)
{
  check_args(state, 1, TYPE_PAIR); /* the environment */
  bind_procs(state, proctab, syntab);
  RET0();
}


const label_t sc_unit_scport = &unit_entry;
