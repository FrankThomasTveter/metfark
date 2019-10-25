/**
* Copyright 1981-2012 ECMWF. 
* 
* This software is licensed under the terms of the GNU Lesser 
* General Public License Version 3 which can be obtained at 
* http://www.gnu.org/licenses/lgpl.html.  
* 
* In applying this licence, ECMWF does not waive the privileges 
* and immunities granted to it by virtue of its status as an 
* intergovernmental organisation nor does it submit to any
* jurisdiction. 
*/

/*
// gbyte.c
*/
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>

#include "pbio-if.h"
#include "fortint.h"

/* 
//  Retrieve or store arbitrary bit-size values from or to SWORD-bit words
//
//  Rewritten April 2000, J.D.Chambers, ECMWF.
//
*/

void gbytes_( void *, void *, fortint *, fortint *, fortint *, fortint *);
void gbytes__( void *, void *, fortint *, fortint *, fortint *, fortint *);
void gbytes( void *, void *, fortint *, fortint *, fortint *, fortint *);
void gbyte_( void* , void *, fortint *, fortint *);
void gbyte__( void* , void *, fortint *, fortint *);
void gbyte( void* , void *, fortint *, fortint *);
void sbytes_( void* , void *, fortint *, fortint *, fortint *, fortint *);
void sbytes__( void* , void *, fortint *, fortint *, fortint *, fortint *);
void sbytes( void* , void *, fortint *, fortint *, fortint *, fortint *);
void sbyte_( void* , void *, fortint *, fortint *);
void sbyte__( void* , void *, fortint *, fortint *);
void sbyte( void* , void *, fortint *, fortint *);
#define BITS_PER_WORD JBPW_DEF
#define SWORD JBPW_DEF

static fortint MASK = -1;                             /* Mask of sword bits */

# define VALUE(p,q,b) \
 (((b)==BITS_PER_WORD ? MASK : ~(MASK<<(b))) & ((p)>>(SWORD-((q)+(b)))))
/*
// Gets a b-bit value, with its leftmost bit at position q
// (q = 0 for most significant bit)
*/

# define MASKVALUE(q,b) \
 ((b)==BITS_PER_WORD ? MASK : (~(MASK<<(b))<<(SWORD-((q)+(b)))))
/*
// Creates a b-bit mask, with its leftmost bit at position q
// (q = 0 for most significant bit)
*/

void gbytes_(
  void *Source,
  void *Destination,
  fortint *startSkip,
  fortint *bitsPerValue,
  fortint *skipBetweenValues,
  fortint *numberOfValues) {
/*
//  GBYTES: 
//
//   Unpacks values from source to destination.
//
//   Makes an initial skip over bits in source, then skips bits between values.
//
//   startSkip >= 0            Number of bits to be skipped preceding first
//                             value in source
//
//   0 < bitsPerValue < 32     Size in bits of each value
//
//   skipBetweenValues >= 0    Number of bits to be skipped between values
//
//   numberOfValues >= 0       Number of values to be packed/unpacked
//
*/
fortint * source = (fortint *) Source;
fortint * destination = (fortint *) Destination;
fortint nextWord,nextValueFirstBit,next;
fortint swordv=SWORD;

  nextWord = 0;
  nextValueFirstBit = *startSkip;
  for (next = 0; next < *numberOfValues; ++next) {
    gbyte_(&source[nextWord],&destination[next],&nextValueFirstBit,bitsPerValue);
    nextValueFirstBit += *bitsPerValue + *skipBetweenValues;
    nextWord += nextValueFirstBit / BITS_PER_WORD;
    nextValueFirstBit %= SWORD;
  }
}

void gbytes__(
  void *Source,
  void *Destination,
  fortint *startSkip,
  fortint *bitsPerValue,
  fortint *skipBetweenValues,
  fortint *numberOfValues) {

  gbytes_(Source,Destination,startSkip,bitsPerValue,skipBetweenValues,
          numberOfValues);
}

void gbytes(
  void *Source,
  void *Destination,
  fortint *startSkip,
  fortint *bitsPerValue,
  fortint *skipBetweenValues,
  fortint *numberOfValues) {

  gbytes_(Source,Destination,startSkip,bitsPerValue,skipBetweenValues,
          numberOfValues);
}

void gbyte_(
  void* Source,
  void *Destination,
  fortint *nextValueFirstBit,
  fortint *bitsPerValue) {
/*
//  GBYTE: 
//
//   Unpacks one value from source to destination.
//
//   nextValueFirstBit >= 0    Number in word of first bit to process
//
//   0 < bitsPerValue < SWORD  Size in bits of each value
//
//   Value may overlap a word boundary
*/
fortint * source = (fortint *) Source;
fortint * destination = (fortint *) Destination;
fortint countOfLeftmostBits,nextWord,leftmostBits,startBit,remainingBits;

/*
//  Position at start word and bit
*/
  startBit      = *nextValueFirstBit;
  remainingBits = *bitsPerValue;

  if (startBit >= SWORD) {
    nextWord = startBit/SWORD;
    startBit %= SWORD;
  } else
    nextWord=0;

/*
//  If value spans a word boundary, pick up leftmost bits first
*/
  countOfLeftmostBits = startBit + remainingBits;
  if (countOfLeftmostBits > SWORD) {
    countOfLeftmostBits = SWORD - startBit;
    remainingBits -= countOfLeftmostBits;
    leftmostBits =
      (VALUE(source[nextWord],startBit,countOfLeftmostBits)) << remainingBits;
    startBit = 0;
    nextWord++;
  } else
    leftmostBits = 0;

/*
//  Complete value by picking up the rightmost bits
*/
  *destination = leftmostBits +
                 (VALUE(source[nextWord],startBit,remainingBits));

}

void gbyte__(
  void* Source,
  void *Destination,
  fortint *nextValueFirstBit,
  fortint *bitsPerValue) {

  gbyte_(Source,Destination,nextValueFirstBit,bitsPerValue);
}

void gbyte(
  void* Source,
  void *Destination,
  fortint *nextValueFirstBit,
  fortint *bitsPerValue) {

  gbyte_(Source,Destination,nextValueFirstBit,bitsPerValue);
}

void sbytes_(
  void* Destination,
  void *Source,
  fortint *startSkip,
  fortint *bitsPerValue,
  fortint *skipBetweenValues,
  fortint *numberOfValues) {
/*
//  SBYTES: 
//
//   Packs values from source to destination.
//
//   Makes an initial skip over bits in source, then skips bits between values.
//
//   startSkip >= 0            Number of bits to be skipped preceding first
//                             value in destination
//
//   0 < bitsPerValue < 32     Size in bits of each value
//
//   skipBetweenValues >= 0    Number of bits to be skipped between values
//
//   numberOfValues >= 0       Number of values to be packed/unpacked
//
*/
fortint * source = (fortint *) Source;
fortint * destination = (fortint *) Destination;
fortint nextWord,nextValueFirstBit,next;

  nextWord = 0;
  nextValueFirstBit = *startSkip;
  for (next = 0; next < *numberOfValues; ++next) {
    sbyte_(&destination[nextWord],&source[next],&nextValueFirstBit,bitsPerValue);
    nextValueFirstBit += *bitsPerValue + *skipBetweenValues;
    nextWord += nextValueFirstBit/SWORD;
    nextValueFirstBit %= SWORD;
  }
}

void sbytes(
  void* Destination,
  void *Source,
  fortint *startSkip,
  fortint *bitsPerValue,
  fortint *skipBetweenValues,
  fortint *numberOfValues) {

  sbytes_(Destination,Source,startSkip,bitsPerValue,skipBetweenValues,
          numberOfValues);
}

void sbytes__(
  void* Destination,
  void *Source,
  fortint *startSkip,
  fortint *bitsPerValue,
  fortint *skipBetweenValues,
  fortint *numberOfValues) {

  sbytes_(Destination,Source,startSkip,bitsPerValue,skipBetweenValues,
          numberOfValues);
}

void sbyte_(
  void* Destination,
  void *Source,
  fortint *nextValueFirstBit,
  fortint *bitsPerValue) {
/*
//  SBYTE: 
//
//   Packs one value from source to destination.
//
//   nextValueFirstBit >= 0    Number in word of first bit to store
//
//   0 < bitsPerValue < SWORD  Size in bits of each value
//
//   Value may overlap a word boundary
//
*/
fortint * source = (fortint *) Source;
fortint * destination = (fortint *) Destination;
fortint countOfLeftmostBits,nextWord,startBit,remainingBits,rightmostBits;

/*
//  Position at start word and bit
*/
  startBit = *nextValueFirstBit;
  remainingBits = *bitsPerValue;

  if (startBit >= SWORD) {
    nextWord = startBit / SWORD;
    startBit %= SWORD;
  } else
    nextWord = 0;

/*
//  If value spans a word boundary, store leftmost bits first
*/
  countOfLeftmostBits = startBit + remainingBits;
  if (countOfLeftmostBits > SWORD) {
    countOfLeftmostBits = SWORD - startBit;
    startBit = SWORD - remainingBits;
    remainingBits -= countOfLeftmostBits;
    destination[nextWord] =
      ((destination[nextWord] >> countOfLeftmostBits) << countOfLeftmostBits)
      + (VALUE(*source,startBit,countOfLeftmostBits));
    startBit = 0;
    nextWord++;
  }

/*
//  Complete value by storing the rightmost bits
*/
  rightmostBits = VALUE(*source,SWORD-remainingBits,remainingBits);
  destination[nextWord] =
    (destination[nextWord] & ~MASKVALUE(startBit,remainingBits))
    + (rightmostBits << SWORD-(remainingBits+startBit));

}

void sbyte(
  void* Destination,
  void *Source,
  fortint *nextValueFirstBit,
  fortint *bitsPerValue) {

  sbyte_(Destination,Source,nextValueFirstBit,bitsPerValue);
}

void sbyte__(
  void* Destination,
  void *Source,
  fortint *nextValueFirstBit,
  fortint *bitsPerValue) {

  sbyte_(Destination,Source,nextValueFirstBit,bitsPerValue);
}
