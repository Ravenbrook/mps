Package Name:
   HIST

Description:
   Simple histogram facility using Format strings for output.

Author:
   Scott E. Fahlman

Address:
   Carnegie-Mellon University
   Computer Science Department
   Pittsburgh, PA 15213

Net Address:
   Scott.Fahlman@CS.CMU.EDU

Copyright Status:
   Public Domain.

Files:
   hist.lisp, hist.fasl, hist.catalog

How to Get:
   The following unix command will copy the pertinent files into directory
<spec>.
   cp /afs/cs.cmu.edu/project/slisp/library/hist/* <spec>

Portability:
   Should run in any legal Common Lisp.

Instructions:
   Hist is a macro of form (HIST (min max [bucket-size]) . body)

Creates a histogram with buckets of the specified size (defaults to 1),
spanning the range from Low (inclusive) to High (exclusive), with two
additional buckets to catch values below and above this range.  The body is
executed as a progn, and every call to Hist-Record within the body provides a
value for the histogram to count.  When Body exits, the histogram is printed
out and Hist returns Nil.

A simple example:
   (hist (0 10) (dotimes (i 1000) (random 10)))
This example may make the RANDOM distribution look more normal:
   (hist (0 10 2) (dotimes (i 1000) (random 10)))
This example will show you overflow buckets:
   (hist (2 12) (dotimes (i 1000) (random 15)))

Wish List:
   Some sort of automatic scaling for the number and size of buckets would be
nice, if the user chooses not to supply these.  This would probably require
running the body twice, once to determine the spread of values, and again to
actually produce the histogram.
