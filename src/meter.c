/* impl.c.meter: METERS
 *
 * $HopeName: $
 * Copyright (C) 1998 Harlequin Group plc.  All rights reserved.
 *
 */

#include "mpm.h"
#include "meter.h"

void MeterInit(Meter meter, char *name) 
{
  meter->name = name;
  meter->count = 0;
  meter->total = 0.0;
  meter->meanSquared = 0.0;
}


void MeterAccumulate(Meter meter, Size amount)
{
  Count count = meter->count + 1;
  double total = meter->total;
  double meanSquared = meter->meanSquared;
  double weight = (double)(count - 1) / (double)count;

  /* .limitation.variance: This computation accumulates a running
   * mean^2, minimizing overflow, but sacrificing numerical stablity
   * for small variances.  For more accuracy, the data set should be
   * emitted using a telemetry stream and analyzed off line.
   */
  meter->count = count;
  meter->total = total + amount;
  meter->meanSquared = weight * meanSquared + amount * amount / count;
}


/* --- crude hack */
static Res WriteDouble(char *before, double d, char* after,
                       mps_lib_FILE *stream)
{
  WriteFU i, f;

  i = (WriteFU)d;
  f = (WriteFU)((d - (double)i) * 1000);
  return
    WriteF(stream, "$S$U.$U$S", before, i, f, after, NULL);
}


Res MeterWrite(Meter meter, mps_lib_FILE *stream)
{
  Res res;

  res = WriteF(stream,
               "meter $S {\n", meter->name,
               "  count: $U\n", meter->count,
               NULL);
  if (res != ResOK)
    return res;
  res = WriteDouble("  total: ", meter->total, "\n", stream);
  if (res != ResOK)
    return res;
  res = WriteDouble("  mean: ", meter->total / meter->count, "\n",
                    stream);
  if (res != ResOK)
    return res;
  /* --- stddev = sqrt(meanSquared - mean^2), but see
   * .limitation.variance above
   */
  res = WriteDouble("  mean^2: ", meter->meanSquared, "\n", stream);
  if (res != ResOK)
    return res;
  res = WriteF(stream,"}\n", NULL);
  if (res != ResOK)
    return res;
  
  return ResOK;
}
