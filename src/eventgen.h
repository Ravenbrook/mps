/* impl.h.eventgen -- Automatic event header
 *
 * Copyright (C) 1997 Harlequin Group, all rights reserved.
 * $HopeName$
 *
 * !!! DO NOT EDIT THIS FILE !!!
 * This file was generated by $HopeName$
 */

#ifdef EVENT

typedef struct {
  Word code;
  Word length;
  Word clock;
} EventStruct;

typedef struct {
  Word code;
  Word length;
  Word clock;
  struct AddrStruct * a0;
} EventAStruct;

typedef struct {
  Word code;
  Word length;
  Word clock;
  struct AddrStruct * a0;
  Word w1;
} EventAWStruct;

typedef struct {
  Word code;
  Word length;
  Word clock;
  void * p0;
} EventPStruct;

typedef struct {
  Word code;
  Word length;
  Word clock;
  void * p0;
  struct AddrStruct * a1;
  struct AddrStruct * a2;
} EventPAAStruct;

typedef struct {
  Word code;
  Word length;
  Word clock;
  void * p0;
  struct AddrStruct * a1;
  Word w2;
} EventPAWStruct;

typedef struct {
  Word code;
  Word length;
  Word clock;
  void * p0;
  void * p1;
} EventPPStruct;

typedef struct {
  Word code;
  Word length;
  Word clock;
  void * p0;
  void * p1;
  struct AddrStruct * a2;
  unsigned u3;
} EventPPAUStruct;

typedef struct {
  Word code;
  Word length;
  Word clock;
  void * p0;
  void * p1;
  struct AddrStruct * a2;
  Word w3;
  void * p4;
} EventPPAWPStruct;

typedef struct {
  Word code;
  Word length;
  Word clock;
  void * p0;
  void * p1;
  void * p2;
} EventPPPStruct;

typedef struct {
  Word code;
  Word length;
  Word clock;
  void * p0;
  void * p1;
  unsigned u2;
} EventPPUStruct;

typedef struct {
  Word code;
  Word length;
  Word clock;
  void * p0;
  void * p1;
  Word w2;
} EventPPWStruct;

typedef struct {
  Word code;
  Word length;
  Word clock;
  void * p0;
  void * p1;
  Word w2;
  struct AddrStruct * a3;
  Word w4;
} EventPPWAWStruct;

typedef struct {
  Word code;
  Word length;
  Word clock;
  void * p0;
  Word w1;
  Word w2;
} EventPWWStruct;

typedef struct {
  Word code;
  Word length;
  Word clock;
  unsigned u0;
  unsigned u1;
  void * p2;
  void * p3;
  void * p4;
} EventUUPPPStruct;

typedef struct {
  Word code;
  Word length;
  Word clock;
  Word w0;
  char s[EventMaxStringLength];
} EventWSStruct;


typedef union {
  EventStruct any;
  EventAStruct a;
  EventAWStruct aw;
  EventPStruct p;
  EventPAAStruct paa;
  EventPAWStruct paw;
  EventPPStruct pp;
  EventPPAUStruct ppau;
  EventPPAWPStruct ppawp;
  EventPPPStruct ppp;
  EventPPUStruct ppu;
  EventPPWStruct ppw;
  EventPPWAWStruct ppwaw;
  EventPWWStruct pww;
  EventUUPPPStruct uuppp;
  EventWSStruct ws;
} EventUnion;


#define EVENT_0(type) \
  EVENT_BEGIN(type, 0, sizeof(EventStruct)) \
  EVENT_END(type, sizeof(EventStruct))

#define EVENT_A(type, _a0) \
  BEGIN \
    size_t _length = sizeof(EventAStruct); \
    EVENT_BEGIN(type, A, _length); \
    Event.a.a0 = (_a0); \
    EVENT_END(type, _length); \
  END

#define EVENT_AW(type, _a0, _w1) \
  BEGIN \
    size_t _length = sizeof(EventAWStruct); \
    EVENT_BEGIN(type, AW, _length); \
    Event.aw.a0 = (_a0); \
    Event.aw.w1 = (_w1); \
    EVENT_END(type, _length); \
  END

#define EVENT_P(type, _p0) \
  BEGIN \
    size_t _length = sizeof(EventPStruct); \
    EVENT_BEGIN(type, P, _length); \
    Event.p.p0 = (_p0); \
    EVENT_END(type, _length); \
  END

#define EVENT_PAA(type, _p0, _a1, _a2) \
  BEGIN \
    size_t _length = sizeof(EventPAAStruct); \
    EVENT_BEGIN(type, PAA, _length); \
    Event.paa.p0 = (_p0); \
    Event.paa.a1 = (_a1); \
    Event.paa.a2 = (_a2); \
    EVENT_END(type, _length); \
  END

#define EVENT_PAW(type, _p0, _a1, _w2) \
  BEGIN \
    size_t _length = sizeof(EventPAWStruct); \
    EVENT_BEGIN(type, PAW, _length); \
    Event.paw.p0 = (_p0); \
    Event.paw.a1 = (_a1); \
    Event.paw.w2 = (_w2); \
    EVENT_END(type, _length); \
  END

#define EVENT_PP(type, _p0, _p1) \
  BEGIN \
    size_t _length = sizeof(EventPPStruct); \
    EVENT_BEGIN(type, PP, _length); \
    Event.pp.p0 = (_p0); \
    Event.pp.p1 = (_p1); \
    EVENT_END(type, _length); \
  END

#define EVENT_PPAU(type, _p0, _p1, _a2, _u3) \
  BEGIN \
    size_t _length = sizeof(EventPPAUStruct); \
    EVENT_BEGIN(type, PPAU, _length); \
    Event.ppau.p0 = (_p0); \
    Event.ppau.p1 = (_p1); \
    Event.ppau.a2 = (_a2); \
    Event.ppau.u3 = (_u3); \
    EVENT_END(type, _length); \
  END

#define EVENT_PPAWP(type, _p0, _p1, _a2, _w3, _p4) \
  BEGIN \
    size_t _length = sizeof(EventPPAWPStruct); \
    EVENT_BEGIN(type, PPAWP, _length); \
    Event.ppawp.p0 = (_p0); \
    Event.ppawp.p1 = (_p1); \
    Event.ppawp.a2 = (_a2); \
    Event.ppawp.w3 = (_w3); \
    Event.ppawp.p4 = (_p4); \
    EVENT_END(type, _length); \
  END

#define EVENT_PPP(type, _p0, _p1, _p2) \
  BEGIN \
    size_t _length = sizeof(EventPPPStruct); \
    EVENT_BEGIN(type, PPP, _length); \
    Event.ppp.p0 = (_p0); \
    Event.ppp.p1 = (_p1); \
    Event.ppp.p2 = (_p2); \
    EVENT_END(type, _length); \
  END

#define EVENT_PPU(type, _p0, _p1, _u2) \
  BEGIN \
    size_t _length = sizeof(EventPPUStruct); \
    EVENT_BEGIN(type, PPU, _length); \
    Event.ppu.p0 = (_p0); \
    Event.ppu.p1 = (_p1); \
    Event.ppu.u2 = (_u2); \
    EVENT_END(type, _length); \
  END

#define EVENT_PPW(type, _p0, _p1, _w2) \
  BEGIN \
    size_t _length = sizeof(EventPPWStruct); \
    EVENT_BEGIN(type, PPW, _length); \
    Event.ppw.p0 = (_p0); \
    Event.ppw.p1 = (_p1); \
    Event.ppw.w2 = (_w2); \
    EVENT_END(type, _length); \
  END

#define EVENT_PPWAW(type, _p0, _p1, _w2, _a3, _w4) \
  BEGIN \
    size_t _length = sizeof(EventPPWAWStruct); \
    EVENT_BEGIN(type, PPWAW, _length); \
    Event.ppwaw.p0 = (_p0); \
    Event.ppwaw.p1 = (_p1); \
    Event.ppwaw.w2 = (_w2); \
    Event.ppwaw.a3 = (_a3); \
    Event.ppwaw.w4 = (_w4); \
    EVENT_END(type, _length); \
  END

#define EVENT_PWW(type, _p0, _w1, _w2) \
  BEGIN \
    size_t _length = sizeof(EventPWWStruct); \
    EVENT_BEGIN(type, PWW, _length); \
    Event.pww.p0 = (_p0); \
    Event.pww.w1 = (_w1); \
    Event.pww.w2 = (_w2); \
    EVENT_END(type, _length); \
  END

#define EVENT_UUPPP(type, _u0, _u1, _p2, _p3, _p4) \
  BEGIN \
    size_t _length = sizeof(EventUUPPPStruct); \
    EVENT_BEGIN(type, UUPPP, _length); \
    Event.uuppp.u0 = (_u0); \
    Event.uuppp.u1 = (_u1); \
    Event.uuppp.p2 = (_p2); \
    Event.uuppp.p3 = (_p3); \
    Event.uuppp.p4 = (_p4); \
    EVENT_END(type, _length); \
  END

#define EVENT_WS(type, _w0, _s) \
  BEGIN \
    const char *_s2;\
    size_t _string_length, _length;\
    _s2 = (_s); \
    _string_length = StringLength((_s2)); \
    AVER(_string_length < EventMaxStringLength);\
    _length = \
      WordAlignUp(offsetof(EventWSStruct, s) + \
                  _string_length + 1, sizeof(Word)); \
    EVENT_BEGIN(type, WS, _length); \
    Event.ws.w0 = (_w0); \
    MPS_MEMCPY(Event.ws.s, (_s2), _string_length + 1); \
    EVENT_END(type, _length); \
  END

#define EventFormat0 0
#define EventFormatA 1
#define EventFormatAW 2
#define EventFormatP 3
#define EventFormatPAA 4
#define EventFormatPAW 5
#define EventFormatPP 6
#define EventFormatPPAU 7
#define EventFormatPPAWP 8
#define EventFormatPPP 9
#define EventFormatPPU 10
#define EventFormatPPW 11
#define EventFormatPPWAW 12
#define EventFormatPWW 13
#define EventFormatUUPPP 14
#define EventFormatWS 15

#else /* EVENT not */

#define EVENT_0(type)    NOOP
#define EVENT_A(type, p0)    NOOP
#define EVENT_AW(type, p0, p1)    NOOP
#define EVENT_P(type, p0)    NOOP
#define EVENT_PAA(type, p0, p1, p2)    NOOP
#define EVENT_PAW(type, p0, p1, p2)    NOOP
#define EVENT_PP(type, p0, p1)    NOOP
#define EVENT_PPAU(type, p0, p1, p2, p3)    NOOP
#define EVENT_PPAWP(type, p0, p1, p2, p3, p4)    NOOP
#define EVENT_PPP(type, p0, p1, p2)    NOOP
#define EVENT_PPU(type, p0, p1, p2)    NOOP
#define EVENT_PPW(type, p0, p1, p2)    NOOP
#define EVENT_PPWAW(type, p0, p1, p2, p3, p4)    NOOP
#define EVENT_PWW(type, p0, p1, p2)    NOOP
#define EVENT_UUPPP(type, p0, p1, p2, p3, p4)    NOOP
#define EVENT_WS(type, p0, p1)    NOOP

#endif /* EVENT */
