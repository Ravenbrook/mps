/* $Header: /project/cmucl/cvsroot/src/lisp/parse.h,v 1.2 2005/01/13 19:55:00 fgilham Exp $ */

#ifndef _PARSE_H_
#define _PARSE_H_


/* All parse routines take a char ** as their only argument */

extern boolean more_p(char **ptr);
extern char *parse_token(char **ptr);
extern lispobj parse_lispobj(char **ptr);
extern char *parse_addr(char **ptr);
extern long parse_number(char **ptr);

#endif /* _PARSE_H_ */
