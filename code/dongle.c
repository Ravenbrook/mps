/* impl.c.dongle: SENTINEL SUPERPRO DONGLE INTERFACE
 *
 * $Id$
 * Copyright (C) 2000 Harlequin Limited.  All rights reserved.
 *
 * .readership: NOTE: This file contains the secret IDs and formats for
 * ScriptWorks and MPS dongles.  It must not be given to outsiders, not
 * even Functional Objects!
 *
 * .from: Adapted from SWsecurity!src:rnbwdong.c(1.32), except the
 * ketchup test in testSuperpro which is from version 1.17.
 */

#include "dongle.h"
#include "check.h" /* for AVER */
#include "mpstd.h"
#if defined(MPS_OS_W3)
#include "mpswin.h" /* needed by spromeps.h */
#endif
#include <SPROMEPS.H>              /* SuperPro API function prototypes     */
#include <time.h> /* for time */

SRCID(dongle, "$Id$");


/* MPS-specific part of dongle format */

/* The iConstants are indexes of the fields: */
/*   version field to allow future changes in format */
#define iDongleVersion 0
/*   customer id field */
#define iCustomerId 1
/*   random configuration options field */
#define iConfig 2
/*   serial number of dongle */
#define iSerial 3
#define RESULT_ARRAY_SIZE 4


/* Compatibility */

#define HQASSERT(test, msg) AVER(test)
#define int32 int
#define uint32 unsigned int
#define uint8 unsigned char
#define RIGOROUS_PRODUCT_CODE_CHECKING


/* DoCustomerNumberCheck -- check that customer ID matches
 *
 * Checks that the customer ID in the dongle (in the dongleValues array)
 * matches the one configured for this build.  A customer ID of 0
 * indicates a "global" dongle that will activate any build.
 *
 * The other two arguments exist for interface compatibility with the SW
 * code, they are not used in MPS.
 */

static int32 DoCustomerNumberCheck(int32* dongleValues, int32* customerID,
                                   int32 fUpdate)
{
  int32 dongleId;

  AVER(fUpdate == FALSE);
  AVER(customerID == NULL);
  dongleId = dongleValues[iCustomerId];
  /* global dongles have customerID 0 */
  return (dongleId == 0 || dongleId  == DONGLE_CUSTOMER_ID);
}


/*
  SuperPro stuff
*/


RB_SPRO_APIPACKET gSproApiPacket;  /* SuperPro packet                      */


#define SPRO_DEVELOPER_ID 0x4567

#define SPRO_DATA_BASE_ADDRESS 0x21
   /* data is placed in cells 0x21 to 0x27 in the dongle */
#define SPRO_QUERY_ADDRESS 0x12

/* Product code, and the cell in which it's stored */
#define SPRO_MPS_PRODUCT_CODE 0x1958
#define SPRO_PRODUCT_ADDRESS    0x08

#define SPRO_MAX_ADDRESS        63

/* How many times to retry a read */
#define SPRO_READ_RETRIES       2


/* -------- Function Prototypes -------- */

static int32 fSPROReadDongleData(int32 * quantums);
static int32 fSPROReadDongle(
    uint32          nAddress,
    RB_WORD*        pValue);


/* -------- Private Data -------- */

static uint8 scrambled_eggs [] = {
    /*  0 */ 0x12, 
    /*  1 */ 0x15, 
    /*  2 */ 0x88, 
    /*  3 */ 0x45,
    /*  4 */ 0x10, 
    /*  5 */ 0xc7,
    /*  6 */ 0x77,
    /*  7 */ 0x64,
    /*  8 */ 0x36, 
    /*  9 */ 0xaf,
    /* 10 */ 0xc4,
    /* 11 */ 0xd6,
    /* 12 */ 0x63,
    /* 13 */ 0xfa,
    /* 14 */ 0x62,
    /* 15 */ 0xba
};

/* initial values not used, just there to confuse the issue */
static uint8 ketchup [] = { 
    0x10, 0xc7, 0x77, 0x64
};

static uint32 hash_browns [] = {
  0xe7, 0xb4, 0xc5, 0x4f, 0x28, 0x22, 0x9a, 0x26,
  0x87, 0xae, 0x5f, 0x88, 0xad, 0xf9, 0x46, 0xb6,

  0x1c, 0x35, 0x55, 0x4a, 0x7b, 0x38, 0xec, 0x9e,
  0xce, 0x2d, 0xfb, 0x4b, 0x6e, 0x2d, 0xb7, 0xb7,

  0xdb, 0xaf, 0x0e, 0x64, 0xcd, 0x4f, 0xe6, 0x19,
  0xec, 0x46, 0x49, 0xa6, 0xef, 0x0d, 0xd9, 0x5f,

  0x6e, 0xe9, 0xe8, 0x92, 0xd4, 0xca, 0xc0, 0x3a,
  0x73, 0xb7, 0xf3, 0x1a, 0xbd, 0xc4, 0x1c, 0x9e
};


static int32 spro_initialised = 0;


int32 fullTestSuperpro (int32 *quantums)
{
  char input [4];
  unsigned long remainder;

  if (! spro_initialised) {

#ifdef _PPC_
	/* If we are using the new API, then format the packet
	*/
    if (RNBOsproFormatPacket(&gSproApiPacket, SPRO_APIPACKET_SIZE))
      return FALSE;
#endif

    if (RNBOsproInitialize( &gSproApiPacket ))
      return FALSE;
    /* try twice, just in case */
    if (RNBOsproFindFirstUnit( &gSproApiPacket, SPRO_DEVELOPER_ID) != SP_SUCCESS)
      if (RNBOsproFindFirstUnit( &gSproApiPacket, SPRO_DEVELOPER_ID) != SP_SUCCESS)
        return FALSE;
    /* is it an MPS dongle? */
    for (;;) {
      RB_WORD ProductCode = 0;
      
      if( !fSPROReadDongle(SPRO_PRODUCT_ADDRESS, &ProductCode) ) {
        return FALSE;
      }
      if (ProductCode == SPRO_MPS_PRODUCT_CODE)
      {
        /* This is an MPS dongle. Read all its data in. */
        if (! fSPROReadDongleData(quantums))
          return FALSE;

        /* now do a pre-test on this data to see if it has at least the
         * right customer number. If not, look for other dongles in 
         * the chain.
         */
        if (DoCustomerNumberCheck(quantums, NULL, FALSE))
          break;
      }
      if (RNBOsproFindNextUnit( &gSproApiPacket ) != SP_SUCCESS)
        return FALSE;
    }
    spro_initialised = (time (NULL) & 0xf) + 1; /* seed */
  }
  else
  {
    /* re-read the seven data words */
    if (! fSPROReadDongleData(quantums))
      return FALSE;
  }

  /* make breakfast */
  input [0] = scrambled_eggs [spro_initialised - 1];
  spro_initialised += 3;
  if (spro_initialised > sizeof (scrambled_eggs))
    spro_initialised -= sizeof (scrambled_eggs);
  input [1] = scrambled_eggs [spro_initialised - 1];
  spro_initialised += 1;
  if (spro_initialised > sizeof (scrambled_eggs))
    spro_initialised -= sizeof (scrambled_eggs);
  input [2] = scrambled_eggs [spro_initialised - 1];
  spro_initialised += 4;
  if (spro_initialised > sizeof (scrambled_eggs))
    spro_initialised -= sizeof (scrambled_eggs);
  input [3] = scrambled_eggs [spro_initialised - 1];
  spro_initialised += 9;
  if (spro_initialised > sizeof (scrambled_eggs))
    spro_initialised -= sizeof (scrambled_eggs);
  if (RNBOsproQuery( &gSproApiPacket, SPRO_QUERY_ADDRESS, 
        (RBP_VOID) input, (RBP_VOID) ketchup, & remainder, 4) 
      != SP_SUCCESS)
    return FALSE;
  /* sunny side up */

  return TRUE;
}


/* Test whether a SuperPro is present.
 *
 * This is just a quick test to see if the dongle is still around. A more
 * rigorous check is done in fullTestSuperpro, although the results of that
 * are checked here, to make it harder to reverse-engineer.
 */
int32 testSuperpro (void)
{
  int32 i = spro_initialised - 2;
  int32     fTestSuccess = FALSE;

#ifdef NO_NT_SECURITY

  fTestSuccess = TRUE;

#else /* !NO_NT_SECURITY */

  RB_WORD ProductCode = 0;

  /* fullTestSuperpro calls the necessary initialization functions. */
  /* If we haven't been there, somebody's trying to hack the dongle. */
  if (spro_initialised == 0)
    return FALSE;

  /* check that the last time we made scrambled eggs the yolk didn't burst */
  if (i < 0)
    i += sizeof (scrambled_eggs); /* sic */
  if (ketchup [2] == hash_browns [i]
      && ketchup [3] == hash_browns [i + 16]
      && ketchup [0] == hash_browns [i + 32]
      && ketchup [1] == hash_browns [i + 48]) {
    if (fSPROReadDongle(SPRO_PRODUCT_ADDRESS, &ProductCode)) {
      fTestSuccess = (SPRO_MPS_PRODUCT_CODE == ProductCode);
    }
  }

#endif /* NO_NT_SECURITY */

  return fTestSuccess;
}


/*
 * Read all data bytes used by ScriptWorks from the dongle
 */
static int32 fSPROReadDongleData(int32 * quantums)
{
  int32 i;

  for (i = 0; i < RESULT_ARRAY_SIZE; i++)
  {
    RB_WORD rbwT = 0;
      
    if( fSPROReadDongle(SPRO_DATA_BASE_ADDRESS + i, &rbwT) )
      quantums[i] = (int32)rbwT;
    else
      return FALSE;
  }
  return TRUE;
}


/* Read from the SuperPro dongle */
static int32 fSPROReadDongle(
    uint32          nAddress,
    RB_WORD*        pValue)
{
  int32     fSuccess = FALSE;
  int32     i;

  HQASSERT(nAddress < SPRO_MAX_ADDRESS, "Can't read out of range Spro cell");
  
  /* Rainbow recommend several retries of a read command if it fails first time */
  for( i = 0; 
       !fSuccess && (i < SPRO_READ_RETRIES); ++i ) {
    *pValue = 0;
    fSuccess = ( RNBOsproRead(&gSproApiPacket, (RB_WORD)nAddress, pValue) == SP_SUCCESS );
  }

  return fSuccess;
}


/* Checking functions
 *
 * This bit corresponds to fullTestSecurityDevice and testSecurityDevice
 * in SWsecurity!src:dongle.c.  It's separate in SW, because they use
 * many different security devices, not just Rainbow.
 */

static int32 resultArray[ RESULT_ARRAY_SIZE ] = { 0 } ;
unsigned int dongleCtr = DONGLE_TEST_FREQUENCY;


/* DongleTestFull -- initialize dongle and verify that it's the right kind */

int DongleTestFull(void)
{
  /* Can't do this because check.h doesn't have all the declarations! */
  /* CHECKL(CHECKTYPE(int32, int)); */
  if (!fullTestSuperpro(resultArray))
    return FALSE;

  return (resultArray[iDongleVersion] >= 1
          /* CustomerID already checked. */
          /* Checking that the unused bits of the config word are zero */
          /* allows us to produce dongles that don't enable old releases. */
          && resultArray[iConfig] == 0);
}


/* DongleTest -- check that the dongle is still there */

int DongleTest(void)
{
  dongleCtr = DONGLE_TEST_FREQUENCY;
  return testSuperpro();
}
