/* impl.h.fmtpscon: POSTSCRIPT OBJECT FORMAT CONSTANTS
 *
 * $Id$
 * Copyright (c) 2001 Ravenbrook Limited.
 *
 * .readership: MPS developers, SW developers
 *
 * .purpose: This contains definitions for a simplified version of the
 * PS object format for internal MM tests.
 * 
 * .history: Adapted from SWv20!export:objects.h(1.26),
 * SWv20iface!export:swvalues.h(1.2), SWv20!export:forward.h(1.7), and
 * SWstandard!export:hqtypes.h(1.17).
 */

#ifndef __FMTPSCON_H__
#define __FMTPSCON_H__


#define MACRO_START       do {
#define MACRO_END         } while(0)


/* definitions copied from hqtypes.h */


typedef	signed	 char 			int8;
typedef	signed	 short			int16;
typedef	signed	 int			int32;

typedef unsigned char			uint8;
typedef unsigned short			uint16;
typedef unsigned int			uint32;


/* definitions copied from swvalues.h */


typedef float    USERVALUE ;
typedef double   PATHVALUE ;

#define LONGESTFILENAME   256


/* dummy save level */

extern int8 ps_save_level;

#define theISaveLevel(workingsave) ps_save_level
#define SAVELEVELINC 2


/* dummy alloc mode: always local */

#define theIGLAllocMode(workingsave) ISLOCAL


/* definitions copied from objects.h */


/* ----------------------------------------------------------------------------
   Define PostScript OBJECT's
---------------------------------------------------------------------------- */


/* CONSTANT DEFINITIONS */

#define CANEXEC       (0x20)
#define CANREAD       (0x40)
#define CANWRITE      (0x60)

#define STYPEMASK     (0x0F)
#define ETYPEMASK     (0x1F)
#define ACCEMASK      (0x60)
#define EXECMASK      (0x80)

/* Used to mark an empty place in a dictionary + operator on execution stack */
#define ONOTHING      (0x00)   /* Exec - internal operator */

#define OINTEGER      (0x01)   /* Exec */
#define OREAL         (0x02)   /* Exec */
#define OINFINITY     (0x03)   /* Exec */

#define OOPERATOR     (0x04)   /* Exec */
#define OBOOLEAN      (0x05)   /* Exec */
#define OMARK         (0x06)   /* Exec - stopped mark */
#define ONULL         (0x07)   /* Exec  - pathforall */
#define OFONTID       (0x08)

#define ONAME         (0x09)   /* Exec */

/* NOTE THAT THIS NUMBER REPRESENTS COMPOSITE OBJECTS AND ABOVE */
#define OSAVE         (0x0A)   /* Exec - marks a bgsave on execution stack */
#define ODICTIONARY   (0x0B)   /* Exec */
#define OSTRING       (0x0C)   /* Exec */
#define OFILE         (0x0D)   /* Exec */

#define OARRAY        (0x0E)   /* Exec */
#define OPACKEDARRAY  (0x0F)   /* Exec-beware possible clashes*/

#define OGSTATE       (0x10)
#define OCONDITION    (0x11)
#define OLOCK         (0x12)

#define OINDIRECT     (0x13)

#define OLONGSTRING   (0x14)	/* string len>65535, len in first 4 bytes of
				 * clist, not in len field of OBJECT.  Most
				 * PS operators won't accept this type....
				 */

#define NONE          (0x00)
#define EXECUTE_ONLY  (0x20)
#define READ_ONLY     (0x40)
#define UNLIMITED     (0x60)

#define LITERAL       (0x00)
#define EXECUTABLE    (0x80)

#define ISAFONT       (0x80)

#define MAXPSSTRING	65535
#define MAXPSARRAY	65535
#define MAXPSDICT	65535
#define MAXPSNAME	127

/* Sub NULL OBJECT types */
#define ISINTERPRETEREXIT  0x00
#define ISPATHFORALL       0x01

#define GLOBMASK           (int8)0x01
#define SAVEMASK           (int8)0x3e

#define ISLOCAL            (int8)0x00
#define ISGLOBAL           (int8)0x01

/* STRUCTURE DEFINITIONS */


/* ncache restructured 23/6/93 by julianb to save a bit of space */
typedef struct ncache {
  int16 sid ;
  uint16 len ;
  struct ncache *next ;
  uint8 *clist ;
  int8  pathcode ;
  uint8 operatorclass ;
  int16 namenumber ;
  struct object *dictobj ;
  struct object *dictval ;
  struct ncache *dictsid ;
  struct object *dictcpy ;	/* Saved copy of dictobj */
} NAMECACHE ;

typedef int32 (* OPFUNCTION)( void );

typedef struct operator {
  OPFUNCTION    opcall  ;
  struct ncache *opname ;
} OPERATOR ;


typedef struct object OBJECT;

struct object {
  union {
    struct {
      uint8 tags ;
      int8 mark ;   /* Must be signed - AC */
      uint16 len ;
    } bits ;
    uint32 transfer ;		   /* must be unsigned - PTH */
  } _d0 ;
  union {
    union {
      void *              object_d1_initialiser ;
      /* NB: object_d1_initialiser MUST BE FIRST; ANSI permits static initialisation
       * of unions via the first element.  Some compilers insist that this be addressy
       * flavoured if it is to be initialised to something relocatable, so having a
       * special field, of void *, just for this purpose is cleanest.  See task 8014.
       * Don't ever use this arm of the union, never let its name be said.
       */
      int32               integer ;
      USERVALUE           real    ;
      int32               bool    ;
      int32               fid     ;
      struct operator     *theop  ;
      uint8 	          *clist  ;
      struct object       *alist  ;
      struct object       *dict   ;
      struct dpair        *plist  ;
      struct ncache       *name   ;
      struct filelist     *files  ;
      struct savelist     *save   ;
      struct gframe       *gstate ;
      int32               *other  ;
      int32               xrefid   ;
      struct stream       *stream ;
    } vals ;
    uint32 transfer ;		   /* must be unsigned - PTH */
  } _d1 ;
} ;

typedef struct dpair {
  OBJECT key ;
  OBJECT obj ;
} DPAIR ;

/* EXTERNAL DEFINITIONS */

/* MACROS DEFINITIONS */

/* Macros for moving an object about. */
#define theD0bit(_obj)   (((_obj)._d0).transfer)
#define theID0bit(_obj)  (((_obj)->_d0).transfer)
#define theD1bit(_obj)   (((_obj)._d1).transfer)
#define theID1bit(_obj)  (((_obj)->_d1).transfer)

/* Checking OBJECTS */
#define theSType(_tags)      ((int32)(_tags)&STYPEMASK)
#define theType(_tags)       ((int32)(_tags)&ETYPEMASK)
#define theAccess(_tags)     ((int32)(_tags)&ACCEMASK)
#define theExec(_tags)       ((int32)(_tags)&EXECMASK)

#define canWriteObj(_tags)   (((int32)(_tags)&ACCEMASK)>=CANWRITE)
#define canReadObj(_tags)    (((int32)(_tags)&ACCEMASK)>=CANREAD)
#define canExecObj(_tags)    (((int32)(_tags)&ACCEMASK)>=CANEXEC)

#define isExecObj(_tags)     ((int32)(_tags)&EXECMASK)

#define isSRCompObj(_o)      (theType(theTags((_o)))>=ONAME)
#define isISRCompObj(_o)     (theType(theITags((_o)))>=ONAME)
#define isPSCompObj(_o)      (theType(theTags((_o)))>=OSAVE)
#define isIPSCompObj(_o)     (theType(theITags((_o)))>=OSAVE)

#define theIGLObjMode(_o)    (theIMark((_o))&GLOBMASK)
#define theGLObjMode(_o)     (theMark((_o))&GLOBMASK)

#define SETIGLOBJECT(_o)      theIMark((_o))=(int8)theIGLAllocMode(workingsave)
#define SETGLOBJECT(_o)       theMark((_o))=(int8)theIGLAllocMode(workingsave)
#define SETIGLOBJECTTO(_o,_v) theIMark((_o))=(int8)(_v)
#define NOTISAVEDOBJECT(_o)   (theIMark((_o))<theISaveLevel(workingsave))

#define SETICSAVEDOBJECT(_o,_gl) MACRO_START			 \
  theIMark((_o)) &= ( int8 )GLOBMASK ;				 \
  theIMark((_o)) |= ( int8 )					 \
    ((_gl) && ( theISaveLevel( workingsave ) > SAVELEVELINC )) ? \
        SAVEMASK : theISaveLevel( workingsave ) ;		 \
MACRO_END

#define NEWICSAVEDOBJECT(_o)  SETICSAVEDOBJECT((_o),theIGLAllocMode(workingsave))

#define SETISSAVEDOBJECT(_o,_gl) MACRO_START			 \
  theIMark((_o)) = ( int8 )(					 \
    ((_gl) && ( theISaveLevel( workingsave ) > SAVELEVELINC )) ? \
        SAVEMASK : theISaveLevel( workingsave )) ;		 \
MACRO_END

#define NEWISSAVEDOBJECT(_o)  SETISSAVEDOBJECT((_o),theIGLAllocMode(workingsave))

#define NOTISAVEDGOBJECT(_l)  ((_l)<theISaveLevel(workingsave))

#define SETIGSAVEDOBJECT(_gf,_gl) MACRO_START			 \
  (_gf)->slevel =						 \
    ((_gl) && ( theISaveLevel( workingsave ) > SAVELEVELINC )) ? \
      SAVEMASK : theISaveLevel( workingsave ) ;			 \
MACRO_END

#define NEWIGSAVEDOBJECT(_gf)  SETIGSAVEDOBJECT((_gf),theIGLAllocMode(workingsave))

#define illegalLocalIntoGlobal(_o)                   \
  (isIPSCompObj((_o))&&(theIGLObjMode((_o))==ISLOCAL)&&theISaveLevel(workingsave))

/* ACCESSOR MACRO DEFINITIONS */

#define theTags(_obj)    (((_obj)._d0).bits.tags)
#define theITags(_obj)   (((_obj)->_d0).bits.tags)
#define theLen(_obj)     (((_obj)._d0).bits.len)
#define theILen(_obj)    (((_obj)->_d0).bits.len)
#define theLStrLen(_obj)  (*((int32*)((_obj)._d1).vals.clist))
#define theILStrLen(_obj) (*((int32*)((_obj)->_d1).vals.clist))
#define theMark(_obj)    (((_obj)._d0).bits.mark)
#define theIMark(_obj)   (((_obj)->_d0).bits.mark)
#define theValue(_obj)   (((_obj)._d1).vals)
#define theIValue(_obj)  (((_obj)->_d1).vals)

#define theInteger(val)      ((val).integer)
#define theReal(val)         ((val).real)
#define theBool(val)         ((val).bool)
#define theFid(val)          ((val).fid)
#define theOp(val)           ((val).theop)
#define theIOp(val)          ((val)->theop)
#define theCList(val)        ((val).clist)
#define theLStrCList(val)    ((val).clist+4)
#define theOList(val)        ((val).alist)
#define theDict(val)         ((val).dict)
#define theDPList(val)       ((val).plist)
#define theName(val)         ((val).name)
#define theFiles(val)        ((val).files)
#define theSave(val)         ((val).save)
#define theGraphicState(val) ((val).gstate)
#define theOther(val)        ((val).other)
#define theXRefID(val)       ((val).xrefid)
#define theStream(val)       ((val).stream)

/* For an xref object the length contains the generation number. */
#define theGen(_obj)     (((_obj)._d0).bits.len)
#define theIGen(_obj)    (((_obj)->_d0).bits.len)

/* On DICTIONARY-PAIR objects. */
#define theKey(val)     ((val).key)
#define theIKey(val)    ((val)->key)
#define theObject(val)  ((val).obj)
#define theIObject(val) ((val)->obj)

/* On OPERATOR objects. */
#define theOpCall(val)        ((val).opcall)
#define theIOpCall(val)       ((val)->opcall)
#define theOpName(val)        ((val).opname)
#define theIOpName(val)       ((val)->opname)

/* On NAME objects. */
#define theNLen(val)   		((val).len)
#define theINLen(val)   	((val)->len)
#define theICList(val)  	((val)->clist)
#define theICachePathcode(val)  ((val)->pathcode)
#define theOpClass(val)         ((val).operatorclass)
#define theIOpClass(val)        ((val)->operatorclass)
#define theINameNumber(val)     ((val)->namenumber)

#define theIDictObj(val)        ((val)->dictobj)
#define theIDictVal(val)        ((val)->dictval)
#define theIDictSid(val)        ((val)->dictsid)

/* copying objects */
#define Copy( o1 , o2 ) \
  ( theID0bit( o1 ) = theID0bit( o2 ) , theID1bit( o1 ) = theID1bit( o2 ))
#define OCopy( o1 , o2 ) \
  ( theD0bit( o1 ) = theD0bit( o2 ) ,   theD1bit( o1 ) = theD1bit( o2 ))


#endif /* protection for multiple inclusion */
