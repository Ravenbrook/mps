/* $Header: /project/cmucl/cvsroot/src/lisp/vars.h,v 1.3 2005/09/15 18:26:53 rtoy Exp $ */

#ifndef _VARS_H_
#define _VARS_H_

extern void flush_vars(void);
extern struct var *lookup_by_name(char *name);
extern struct var *lookup_by_obj(lispobj obj);
extern struct var *define_var(char *name, lispobj obj, boolean perm);
extern struct var *define_dynamic_var(char *name,
				      lispobj update_fn(struct var *var),

				      boolean perm);

extern char *var_name(struct var *var);
extern lispobj var_value(struct var *var);
extern long var_clock(struct var *var);
extern void var_setclock(struct var *var, long value);

#endif /* _VARS_H_ */
