/* impl.h.meter: METER INTERFACE
 *
 * $HopeName: MMsrc!meter.h(MMdevel_mv2_rework.2) $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 *
 * Defines an interface for creating "meters" that accumulate the
 * number, total and mean^2 of a set of data points.  These
 * accumulators can be used to report on the number, total, average,
 * and variance of the data set.
 *
 * .limitation: This computation accumulates a running mean^2,
 * minimizing overflow, but sacrificing numerical stablity for small
 * variances.  For more accuracy, the data set should be emitted using
 * a telemetry stream and analyzed off line.
 *
 */

#ifndef meter_h
#define meter_h

#include "mpm.h"

/* mpmtypes.h */
typedef struct MeterStruct *Meter;


/* mpmst.h */
typedef struct MeterStruct 
{
  char *name;
  Count count;
  double total;
  double meanSquared;
  Size min;
  Size max;
}MeterStruct;


/* mpm.h */
extern void MeterInit(Meter meter, char* name);
extern void MeterAccumulate(Meter meter, Size amount);
extern Res MeterWrite(Meter meter, mps_lib_FILE *stream);


#include "config.h"
#include "misc.h"
#include "mpslib.h"

#define METER_DECL(meter) struct MeterStruct meter
#define METER_INIT(meter, init) STATISTIC(MeterInit(&(meter), (init)))
#define METER_ACC(meter, delta) STATISTIC(MeterAccumulate(&(meter), (delta)))
#define METER_WRITE(meter, stream) STATISTIC(MeterWrite(&(meter), (stream)))

#endif /* meter_h */
