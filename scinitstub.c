/* scinit.c -- dummy "init" unit
 *
 * Richard Brooksby, 2004-08-09
 *
 * $Id$
 *
 * 2004-08-09  RB  Created so that a "nucleus" SC library can be compiled
 * in order to compile "init.c" and link it into a full SC library.
 */

#include "sc.h"


static void unit_entry(state_t state)
{
  check_args(state, 1, TYPE_PAIR); /* the environment */
  RET0();
}


const label_t sc_unit_init = &unit_entry;
