/* scrw.c -- low-level read/write functions
 *
 * Richard Brooksby, 2004-08-04
 *
 * $Id$
 *
 * 2004-08-04  RB  Split off from sc.c code written since 1996-08-30.
 */

#include "sc.h"

#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <errno.h>
#include <stdlib.h>


/* Character names and escapes
 *
 * Character names are used in reading and writing character constants.
 * Only "space" and "newline" are part of R5RS.  Character escapes map
 * the C-like notation for some character codes to their escape codes.
 */

static const struct {
  const char *name;
  const char character;
} charnames[] = {
  {"space",	' '},
  {"newline",	'\n'},
  {"tab",	'\t'},
  {"return",	'\r'},
  {"bell",	'\a'},
  {"backspace", '\b'},
  {"formfeed",	'\f'},
  {"vtab",	'\v'},		/* @@@@ should be "vertical-tab"? */
};

static const char charcodes[] = "\0\a\b\f\n\r\t\v";
static const char charescapes[] = "0abfnrtv";
static const char hexdigits[] = "0123456789ABCDEF";


/* write_char, write_string, write_formatted -- output with error handling */

extern void write_char(state_t state, FILE *stream, char c)
{
  if(putc(c, stream) == EOF)
    error(state, "error writing character to stream: %s", strerror(errno));
}

extern void write_string(state_t state, FILE *stream, const char *string)
{
  if(fputs(string, stream) == EOF)
    error(state, "error writing string to stream: %s", strerror(errno));
}

extern void write_formatted(state_t state, FILE *stream, const char *format, ...)
{
  va_list vargs;
  va_start(vargs, format);
  if(vfprintf(stream, format, vargs) < 0)
    error(state, "error writing formatted output to stream: %s", strerror(errno));
  va_end(vargs);
}

static int read_char(state_t state, FILE *stream)
{
  int c = getc(stream);
  if(c == EOF && ferror(stream))
    error(state, "error reading character from stream: %s", strerror(errno));
  return c;
}

static void unread_char(state_t state, FILE *stream, int c)
{
  if(c != EOF)
    if(ungetc(c, stream) == EOF)
      error(state, "error pushing character onto stream: %s", strerror(errno));
}


/* read_char_nonblank -- get next non-blank char from stream */

static int read_char_nonblank(state_t state, FILE *stream)
{
  int c;
  repeat {
    c = read_char(state, stream);

    if(c == ';') {		/* comment? */
      do
	c = read_char(state, stream);
      until(c == EOF || c == '\n');
    }

    if(!isspace(c))
      break;			/* other non-blank? */
  }

  return c;
}


/* write_char_escaped -- write an escaped character to a stream
 *
 * Escapes any non-printable character, the backslash character, or
 * the delimeter character.
 *
 * Strings and characters written with write_char_escaped don't conform
 * to R5RS, which only allows the escapes \" and \\ in strings.  These
 * strings are an extension of R5RS.
 */

static void write_char_escaped(state_t state,
                               FILE *stream,
                               char c,
                               char delimeter)
{
  ASSERT(isprint(delimeter));
  ASSERT(delimeter != '\\');
  if(c == delimeter)
    write_formatted(state, stream, "\\%c", delimeter);
  else if(c == '\\')
    write_string(state, stream, "\\\\");
  else if(isprint(c))
    write_char(state, stream, c);
  else {
    char *p = memchr(charcodes, c, ARRAYLEN(charcodes));
    write_char(state, stream, '\\');
    if(p != NULL)
      write_char(state, stream, charescapes[p - charcodes]);
    else
      /* In C, a hexadecimal character escape continues for as many characters */
      /* a possible [C99, pp59-60].  We demand exactly two. */
      write_formatted(state, stream, "x%02X", (unsigned char)c);
  }
}


/* write_char_named -- write a named or escaped character from a stream
 *
 * Writes a character in Scheme format, using a named character (e.g.
 * "#\newline") if possible.
 */

static void write_char_named(state_t state, FILE *stream, char c)

{
  size_t i;
  write_string(state, stream, "#\\");
  for(i = 0; i < ARRAYLEN(charnames); ++i)
    if(c == charnames[i].character) {
      write_string(state, stream, charnames[i].name);
      return;
    }
  write_char_escaped(state, stream, c, ' ');
}


/* read_char_escaped -- read an escaped character from a stream
 *
 * Reads characters written by write_char_escaped.  If a character matching the
 * delimeter is read, returns zero and leaves *cp untouched, so if
 * no delimter is needed, call like:
 *   (void)read_char_escaped(state, c, stream, what, &c);
 */

static int read_char_escaped(state_t state,
		             FILE *stream,
		             char delimeter,
		             const char *what,
		             int *cp)
{
  int c;
  ASSERT(isprint(delimeter));
  ASSERT(delimeter != '\\');
  c = read_char(state, stream);
  if(c == EOF)
    error(state, "end of file during %s", what);
  if(c == delimeter)
    return 0;
  if(c == '\\') {
    c = read_char(state, stream);
    if(c == EOF)
      error(state, "end of file in escape sequence in %s", what);
    if(c == 'x') {
      int i;
      ulong n = 0;
      /* In C, a hexadecimal character escape continues for as many characters */
      /* a possible [C99, pp59-60].  We demand exactly two. */
      for(i = 0; i < 2; ++i) {
	char *p;
	c = read_char(state, stream);
	if(c == EOF)
	  error(state, "end of file in hexadecimal escape sequence in %s", what);
	p = memchr(hexdigits, toupper(c), ARRAYLEN(hexdigits));
	if(p == NULL)
	  error(state, "illegal hexadecimal digit '%c' in escape sequence in %s", c, what);
	n = (n << 4) | (p - hexdigits);
      }
      c = (char)n;
    } else if(c != '\\' && c != delimeter) {
      char *p = memchr(charescapes, c, ARRAYLEN(charescapes));
      if(p != NULL)
	c = charcodes[p - charescapes];
      else
	error(state, "unknown escape '%c' in %s", c, what);
    }
  }
  *cp = c;
  return 1;
}


/* isinitial, issubsequent -- test for "extended alphabetic" characters
 *
 * isinitial tests if a character is a legal initial character for
 * a Scheme symbol.  issubsequent tests if it is allowed elsewhere
 * in a symbol.	 The definitions are straight from section 7.1.1,
 * "Lexical structure", of R5RS.
 */

static int isinitial(int c)
{
  return isalpha(c) ||
         (c != EOF && strchr("!$%&*/:<=>?^_~", c) != NULL);
}

static int issubsequent(int c)
{
  return isinitial(c) || isdigit(c) ||
         (c != EOF && strchr("+-.@", c) != NULL);
}


/* read
 * read port 
 *
 * "`Read' converts external representations of Scheme objects into
 *  the objects themselves. That is, it is a parser for the
 *  nonterminal <datum> (see sections see section External
 *  representations and see section Pairs and lists). `Read' returns
 *  the next object parsable from the given input port, updating port
 *  to point to the first character past the end of the external
 *  representation of the object.
 *  
 * "If an end of file is encountered in the input before any
 *  characters are found that can begin an object, then an end of file
 *  object is returned. The port remains open, and further attempts to
 *  read will also return an end of file object. If an end of file is
 *  encountered after the beginning of an object's external
 *  representation, but the external representation is incomplete and
 *  therefore not parsable, an error is signalled.
 *  
 * "The port argument may be omitted, in which case it defaults to the
 *  value returned by `current-input-port'. It is an error to read
 *  from a closed port." -- R5RS
 *
 * L0	input port
 * L1	the prefix symbol for the 'x ,x ,@x forms / the list being read
 * L2	the last pair of the list being read
 */

static void read_entry(state_t);
static void read_symbol(state_t);
static void read_funny(state_t);
static void read_quote(state_t);
static void read_quasiquote(state_t);
static void read_unquote(state_t);
static void read_list(state_t);
static void read_special(state_t);
static void read_string(state_t);
static ulong read_integer(state_t state, FILE *stream, unsigned base);

static void read_entry(state_t state)
{
  FILE *stream;
  int c;

  ENTER(3);

  if(ARGS == 0)
    L0 = state->inport;
  else if(ARGS == 1) {
    unless(TYPE(A0) == TYPE_INPORT)
      error(state, "argument must be an input port");
    L0 = A0;
  } else
    error(state, "takes zero or one argument");

  stream = INPORTSTREAM(L0);
  if(stream == NULL)
    error(state, "can't read from closed port");

  c = read_char_nonblank(state, stream);

  if(c == EOF) {
    LEAVE();
    RET1(obj_eof);
  }

  switch(c) {
  case '\'': JUMP(read_quote);
  case '`':  JUMP(read_quasiquote);
  case ',':  JUMP(read_unquote);
  case '(':  JUMP(read_list);
  case '#':  JUMP(read_special);
  case '"':  JUMP(read_string);
  case '|':  JUMP(read_funny);

  case '-': case '+':
    {
      int c2 = read_char(state, stream);
      unread_char(state, stream, c2);
      if(isdigit(c2)) {
	int_t i = read_integer(state, stream, 10);
	MAKE_INTEGER(T0, c == '+' ? i : -i);
	LEAVE();
	RET1(T0);
      } else if(!issubsequent(c2)) {
	LEAVE();
	RET1(c == '+' ? sym_plus : sym_minus);
      } else
	error(state, "ordinary symbols may not start with '%c'", c);
    }

  case '.':			/* this can only mean "..." or an error */
    c = read_char(state, stream);
    if(c == '.') {
      c = read_char(state, stream);
      if(c == '.') {
	c = read_char(state, stream);
	if(!issubsequent(c)) {
	  unread_char(state, stream, c);
	  LEAVE();
	  RET1(sym_dotdotdot);
	}
      }
    }
    error(state, "ordinary symbols may not start with '.'");
  }

  if(isdigit(c)) {
    int_t i;
    unread_char(state, stream, c);
    i = read_integer(state, stream, 10);
    MAKE_INTEGER(T0, i);
    LEAVE();
    RET1(T0);
  }

  if(isinitial(c)) {
    unread_char(state, stream, c);
    JUMP(read_symbol);
  }

  error(state, "illegal character '%c'", c);
}

static ulong read_integer(state_t state, FILE *stream, unsigned base)
{
  char *p;
  int c;
  int_t integer = 0;
  unsigned digit;
  size_t count = 0;

  ASSERT(2 <= base && base <= 16);

  repeat {
    c = read_char(state, stream);
    if(c == EOF)
      break;
    p = strchr(hexdigits, toupper(c));
    if(p == NULL)
      break;
    digit = p - hexdigits;
    if(digit >= base)
      break;
    integer = integer * base + digit;
    ++count;
  }
  unread_char(state, stream, c);

  if(count == 0)
    error(state, "bad numeric constant");

  return integer;
}

static void read_symbol(state_t state)
{
  size_t length = 0;
  char string[SYMMAX+1];
  FILE *stream = INPORTSTREAM(L0);
  int c = read_char(state, stream);

  ASSERT(isinitial(c));

  do {
    string[length++] = tolower(c);
    c = read_char(state, stream);
  } while(length < SYMMAX && issubsequent(c));
  if(issubsequent(c))
    error(state, "symbol too long");
  unread_char(state, stream, c);

  string[length] = '\0';

  MAKE_STRING(T0, length, string);
  LEAVE();
  TAIL1(proc_string_to_sym, T0);
}

static size_t read_delimeted(state_t state,
			     char delimeter,
			     char *string,
			     size_t max_length,
			     const char *what)
{
  size_t length = 0;
  FILE *stream = INPORTSTREAM(L0);
  int c;

  /* the initial delimeter has been read by this point */

  while(read_char_escaped(state, stream, delimeter, what, &c)) {
    if(length >= max_length)
      error(state, "%s too long", what);
    string[length++] = c;
  }

  string[length] = '\0';

  return length;
}

static void read_funny(state_t state)
{
  char string[SYMMAX+1];
  size_t length = read_delimeted(state, '|', string, SYMMAX, "funny symbol");
  MAKE_STRING(T0, length, string);
  LEAVE();
  TAIL1(proc_string_to_sym, T0);
}

static void read_string(state_t state)
{
  char string[STRMAX+1];
  size_t length = read_delimeted(state, '"', string, STRMAX, "string literal");
  MAKE_STRING(T0, length, string);
  LEAVE();
  RET1(T0);
}

static void read_prefix(state_t);

static void read_quote(state_t state)
{
  L1 = sym_quote;
  CALL1(proc_read, L0, read_prefix);
}

static void read_quasiquote(state_t state)
{
  L1 = sym_quasiquote;
  CALL1(proc_read, L0, read_prefix);
}

static void read_unquote(state_t state)
{
  FILE *stream = INPORTSTREAM(L0);
  int c = read_char(state, stream);

  if(c == '@')
    L1 = sym_unquote_splicing;
  else {
    unread_char(state, stream, c);
    L1 = sym_unquote;
  }

  CALL1(proc_read, L0, read_prefix);
}

static void read_prefix(state_t state)
{
  check_results(state, 1, 0);	/* expect one result from read */

  if(A0 == obj_eof)
    error(state, "end of file reading quoted form");

  MAKE_PAIR(T0, A0, obj_empty); /* cons the appropriate prefix */
  MAKE_PAIR(T0, L1, T0);
  LEAVE();
  RET1(T0);
}

static void read_list_while(state_t);
static void read_list_next(state_t state);
static void read_list_loop(state_t);
static void read_list_cdr(state_t);

static void read_list(state_t state)
{
  L1 = obj_empty;		/* the list is accumulated here */
  JUMP(read_list_while);
}

static void read_list_while(state_t state)
{
  FILE *stream = INPORTSTREAM(L0);
  int c = read_char_nonblank(state, stream);

  if(c == ')') {		/* end of the list? */
    LEAVE();
    RET1(L1);
  }

  /* Detect a dotted last pair. */
  /* It's gnarly because it must also deal with the peculiar symbol */
  /* "..." appearing in a list. */
  if(c == '.') {			/* "." so far */
    c = read_char(state, stream);
    if(c == '.')			/* ".." so far */
      if(read_char(state, stream) == '.') { /* "..." so far */
	c = read_char(state, stream);
	if(issubsequent(c))		/* "...x", not allowed */
	  error(state, "ordinary symbols may not begin with '.'");
	unread_char(state, stream, c);	/* "... " or other delimeter */
	T0 = sym_dotdotdot;
	JUMP(read_list_loop);
      }
    if(issubsequent(c))			/* ".x", not allowed */
      error(state, "ordinary symbols may not being with '.'");
    unread_char(state, stream, c);	/* ". " or other delimeter */
    if(L1 == obj_empty)
      error(state, "dot at beginning of list");
    CALL1(proc_read, L0, read_list_cdr);
  }

  unread_char(state, stream, c);

  CALL1(proc_read, L0, read_list_next);
}

static void read_list_next(state_t state)
{
  check_results(state, 1, 0);	/* expect one result from read */

  if(A0 == obj_eof)
    error(state, "end of file reading list");

  T0 = A0;
  JUMP(read_list_loop);
}

static void read_list_loop(state_t state)
{
  MAKE_PAIR(T0, T0, obj_empty);
  if(L1 == obj_empty) {
    L1 = T0;			/* it's the first pair in the list */
    L2 = T0;			/* and it's the last */
  } else {
    SETCDR(L2, T0);		/* append the new pair to the list */
    L2 = T0;			/* and make it the new last pair */
  }

  JUMP(read_list_while);
}

static void read_list_cdr(state_t state)
{
  FILE *stream = INPORTSTREAM(L0);
  int c = read_char_nonblank(state, stream);

  check_results(state, 1, 0);	/* expect one result from read */

  if(A0 == obj_eof)
    error(state, "end of file reading list cdr");

  SETCDR(L2, A0);		/* set the cdr of the list */

  if(c != ')')
    error(state, "expected close parenthesis");

  LEAVE();
  RET1(L1);
}

void read_vector(state_t);

static void read_special(state_t state)
{
  FILE *stream = INPORTSTREAM(L0);
  int c = read_char_nonblank(state, stream);

  if(c == EOF)
    error(state, "end of file reading special form");

  switch(tolower(c)) {

  case 't': LEAVE(); RET1(obj_true);	/* true */
  case 'f': LEAVE(); RET1(obj_false);	/* false */

  case 'b':			/* binary number constant */
    {
      int_t i = read_integer(state, stream, 2);
      MAKE_INTEGER(T0, i);
      LEAVE();
      RET1(T0);
    }

  case 'o':			/* octal number constant */
    {
      int_t i = read_integer(state, stream, 010);
      MAKE_INTEGER(T0, i);
      LEAVE();
      RET1(T0);
    }

  case 'x':			/* hexadecimal number constant */
    {
      int_t i = read_integer(state, stream, 0x10);
      MAKE_INTEGER(T0, i);
      LEAVE();
      RET1(T0);
    }

  case '\\':			/* character (R4RS 6.6) */
    c = read_char(state, stream);
    if(c == EOF)
      error(state, "end of file reading character literal");
    if(c == '\\') {		/* escape */
      unread_char(state, stream, c);
      c = 'x';			/* doesn't matter what */
      (void)read_char_escaped(state, stream, c, "character literal", &c);
    } else if(isalpha(c)) {	/* character name? */
      char name[CHARNAMEMAX+1];
      size_t i = 0;
      char c2 = c;
      do {
	name[i++] = tolower(c2);
	c2 = read_char(state, stream);
      } while(isalpha(c2));
      unread_char(state, stream, c2);
      name[i] = '\0';
      if(i > 1) {
	for(i = 0; i < ARRAYLEN(charnames); ++i)
	  if(strcmp(name, charnames[i].name) == 0) {
	    c = charnames[i].character;
	    goto found;
	  }
	error(state, "unknown character name \"%s\"", name);
	found: NOOP;
      }
    }
    MAKE_CHARACTER(T0, c);
    LEAVE();
    RET1(T0);

  case '(':			/* vector (R4RS 6.8) */
    unread_char(state, stream, c);
    CALL1(proc_read, L0, read_vector);
  }
  error(state, "unknown special form \"#%c\"", c);
}

void read_vector(state_t state)
{
  check_results(state, 1, 0);	/* expect one result from read */

  if(A0 == obj_eof)
    error(state, "end of file reading vector");

  T0 = A0;
  LEAVE();
  TAIL1(proc_list_to_vec, T0);
}


/* isfunny -- is a symbol "funny", i.e. not lexically an identifier */

static int isfunny(obj_t symbol)
{
  size_t i, len;
  char *str;

  ASSERT(TYPE(symbol) == TYPE_SYMBOL);
  len = SYMLEN(symbol);
  if(len == 0)
    return 1;
  str = SYMSTR(symbol);
  /* oddly enough, peculiar symbols are not funny, interned or not */
  if((len == 1 && (str[0] == '+' || str[0] == '-')) ||
     (len == 3 && memcmp(str, "...", 3) == 0))
    return 0;
  if(!isinitial(str[0]))
    return 1;
  for(i = 1; i < len; ++i)
    if(!issubsequent(str[i]))
      return 1;
  return 0;
}


/* write_obj -- write a representation of an object to a stream */

extern void write_obj(state_t state, obj_t obj, unsigned depth, FILE *stream)
{
  switch(TYPE(obj)) {
  case TYPE_INTEGER:
    write_formatted(state, stream, "%ld", INT(obj));
    break;

  case TYPE_SYMBOL:
    if(isfunny(obj)) {
      size_t i;
      write_char(state, stream, '|');
      for(i = 0; i < SYMLEN(obj); ++i)
	write_char_escaped(state, stream, SYMSTR(obj)[i], '|');
      write_char(state, stream, '|');
    } else
      write_string(state, stream, SYMSTR(obj));
    break;

  case TYPE_SPECIAL:
    write_string(state, stream, obj->special.name);
    break;

  case TYPE_INPORT:
    write_formatted(state, stream, "#[input-port %p]", (void *)INPORTSTREAM(obj));
    break;

  case TYPE_OUTPORT:
    write_formatted(state, stream, "#[output-port %p]", (void *)OUTPORTSTREAM(obj));
    break;

  case TYPE_CHARACTER:
    write_char_named(state, stream, obj->character.c);
    break;

  case TYPE_EXCEPTION:
    write_string(state, stream, "#[exception ");
    if(depth == 0)
      write_string(state, stream, "...");
    else
      write_obj(state, obj->exception.object, depth - 1, stream);
    write_char(state, stream, ']');
    break;

  case TYPE_STRING:
    {
      strlen_t i;
      write_char(state, stream, '"');
      for(i = 0; i < obj->string.length; ++i)
	write_char_escaped(state, stream, STR(obj)[i], '"');
      write_char(state, stream, '"');
    }
    break;

  case TYPE_PAIR:
    if(TYPE(CAR(obj)) == TYPE_SYMBOL &&
       TYPE(CDR(obj)) == TYPE_PAIR &&
       CDDR(obj) == obj_empty) {
      if(CAR(obj) == sym_quote) {
	write_char(state, stream, '\'');
	if(depth == 0)
	  write_string(state, stream, "...");
	else
	  write_obj(state, CADR(obj), depth - 1, stream);
	break;
      }
      if(CAR(obj) == sym_quasiquote) {
	write_char(state, stream, '`');
	if(depth == 0)
	  write_string(state, stream, "...");
	else
	  write_obj(state, CADR(obj), depth - 1, stream);
	break;
      }
      if(CAR(obj) == sym_unquote) {
	write_char(state, stream, ',');
	if(depth == 0)
	  write_string(state, stream, "...");
	else
	  write_obj(state, CADR(obj), depth - 1, stream);
	break;
      }
      if(CAR(obj) == sym_unquote_splicing) {
	write_string(state, stream, ",@");
	if(depth == 0)
	  write_string(state, stream, "...");
	else
	  write_obj(state, CADR(obj), depth - 1, stream);
	break;
      }
    }
    write_char(state, stream, '(');
    if(depth == 0)
      write_string(state, stream, "...");
    else {
      obj_t tortoise = obj;
      repeat {
	write_obj(state, CAR(obj), depth - 1, stream);
	obj = CDR(obj);
	if(TYPE(obj) != TYPE_PAIR) break;
	write_char(state, stream, ' ');
	write_obj(state, CAR(obj), depth - 1, stream);
	obj = CDR(obj);
	if(TYPE(obj) != TYPE_PAIR) break;
	write_char(state, stream, ' ');
	tortoise = CDR(tortoise);
	if(tortoise == obj) {
	  write_string(state, stream, " #!circular");
	  obj = obj_empty;
	  break;
	}
      }
      if(obj != obj_empty) {
	write_string(state, stream, " . ");
	write_obj(state, obj, depth - 1, stream);
      }
    }
    write_char(state, stream, ')');
    break;

  case TYPE_VECTOR:
    write_string(state, stream, "#(");
    if(depth == 0)
      write_string(state, stream, "...");
    else {
      strlen_t i;
      for(i = 0; i < VECLEN(obj); ++i) {
	unless(i == 0) write_char(state, stream, ' ');
	write_obj(state, VECREF(obj, i), depth - 1, stream);
      }
    }
    write_char(state, stream, ')');
    break;

  case TYPE_PROC:
    ASSERT(TYPE(obj->proc.name) == TYPE_SYMBOL);
    write_formatted(state, stream,
                    "#[procedure \"%s\" %p",
	            SYMSTR(obj->proc.name),
	            (void *)obj->proc.entry); /* @@@@ assumes symbol not funny */
#ifdef WRITE_PROC
    if(depth == 0)
      write_string(state, stream, " ...");
    else {
      size_t i;
      /* Elide these values if they are undefined, as they */
      /* are for some primitive operators. */
      unless(obj->proc.cont == obj_undef) {
	write_string(state, stream, " cont = ");
	write_obj(obj->proc.cont, depth - 1, stream);
      }
      for(i = 0; i < obj->proc.regs; ++i)
	unless(LOC(obj, i) == obj_undef) {
	  write_formatted(state, stream, " L%u = ", i);
	  write_obj(LOC(obj, i), depth - 1, stream);
	}
    }
#endif /* WRITE_PROC */
    write_char(state, stream, ']');
    break;

  default:
    ASSERT(0);
    abort();
  }
}


/* write -- convert object to external representation
 *
 * (write <object>)
 * (write <object> <port>)
 */

static void write_entry(state_t state)
{
  FILE *stream;
  if(ARGS == 2) {
    unless(TYPE(A1) == TYPE_OUTPORT)
      error(state, "second argument must be an output port");
    stream = OUTPORTSTREAM(A1);
  } else if(ARGS == 1)
    stream = OUTPORTSTREAM(state->outport);
  else
    error(state, "takes one or two arguments, not %lu", (ulong)ARGS);
  if(stream == NULL)
    error(state, "can't write to closed port");
  write_obj(state, A0, WRITE_DEPTH, stream);
  RET0();
}


/* write-string */

static void write_string_entry(state_t state)
{
  FILE *stream;
  strlen_t i;

  if(ARGS == 1)
    stream = OUTPORTSTREAM(state->outport);
  else if(ARGS == 2) {
    unless(TYPE(A1) == TYPE_OUTPORT)
      error(state, "second argument must be an output port");
    stream = OUTPORTSTREAM(A1);
  } else
    error(state, "takes one or two arguments, not %lu", (ulong)ARGS);
  unless(TYPE(A0) == TYPE_STRING)
    error(state, "first argument must be a string");
  if(stream == NULL)
    error(state, "can't write to closed port");

  /* Can't use fputs because the string might contain NULs. */
  for(i = 0; i < STRLEN(A0); ++i)
    write_char(state, stream, STR(A0)[i]);

  RET0();
}


/* proctab -- initial procedure table
 *
 * The procedures listed in this table are initialized and added
 * to the environment by "unit_entry".
 */

static const proctab_s proctab[] = {
  {"read",		read_entry},
  {"write",			write_entry},
  {"write-string",		write_string_entry},
  { NULL, NULL},
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


const label_t sc_unit_scrw = &unit_entry;
