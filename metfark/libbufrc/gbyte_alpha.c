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

#define GBYTES gbytes_
#define GBYTE  gbyte_
#define SBYTES sbytes_
#define SBYTE  sbyte_

#define SWORD 32
#define MASKSWORD 0x3f

static int onbit[32] = {
  0x00000001,0x00000002,0x00000004,0x00000008,
  0x00000010,0x00000020,0x00000040,0x00000080,
  0x00000100,0x00000200,0x00000400,0x00000800,
  0x00001000,0x00002000,0x00004000,0x00008000,
  0x00010000,0x00020000,0x00040000,0x00080000,
  0x00100000,0x00200000,0x00400000,0x00800000,
  0x01000000,0x02000000,0x04000000,0x08000000,
  0x10000000,0x20000000,0x40000000,0x80000000};

/* 
//  Retrieve or store arbitrary bit-size values from or to SWORD-bit words
//
//  Rewritten April 2000, J.D.Chambers, ECMWF.
//
*/

void GBYTES(
  void* Source,
  void* Destination,
  int* startSkip,
  int* bitsPerValue,
  int* skipBetweenValues,
  int* numberOfValues) {
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
//   0 < bitsPerValue < SWORD  Size in bits of each value
//
//   skipBetweenValues >= 0    Number of bits to be skipped between values
//
//   numberOfValues >= 0       Number of values to be packed/unpacked
//
// Move one bit at a time (!!)
// Uses shifts and masks instead of divides.
// Sets destination bit to 0/1 as source bit is 0/1.
*/
unsigned char* source = (unsigned char*) Source;
int* destination = (int*) Destination;
int bpv, bit, byte, bitShift, nextBit, nextValueFirstBit, next, step;

    bpv = *bitsPerValue;
    step = bpv + *skipBetweenValues;
    nextValueFirstBit = *startSkip;

    for (next = 0; next < *numberOfValues; ++next) {
      destination[next] = 0; 
      nextBit = nextValueFirstBit;

      for ( bit = 0; bit < bpv; bit++ ) {
        byte        = ( nextBit ) >> 3;
        bitShift    = 7 - ( nextBit++ ) & 0x7;

        destination[next] <<= 1 ;
        if( (source[byte] & onbit[bitShift]) ) destination[next] |= 1;
      }

      nextValueFirstBit += step;
    }

}

void GBYTES_(
  void* Source,
  void* Destination,
  int* startSkip,
  int* bitsPerValue,
  int* skipBetweenValues,
  int* numberOfValues) {
    GBYTES(Source,Destination,startSkip,bitsPerValue,skipBetweenValues,numberOfValues);
}

void GBYTES__(
  void* Source,
  void* Destination,
  int* startSkip,
  int* bitsPerValue,
  int* skipBetweenValues,
  int* numberOfValues) {
    GBYTES(Source,Destination,startSkip,bitsPerValue,skipBetweenValues,numberOfValues);
}

void GBYTE(
  void* Source,
  void* Destination,
  int* startSkip,
  int* bitsPerValue) {
/*
//  GBYTE: 
//
//   Unpacks one value from source to destination.
//
//   Makes an initial skip over bits in source.
//
//   startSkip >= 0            Number of bits to be skipped preceding first
//                             value in source
//
//   0 < bitsPerValue < SWORD  Size in bits of each value
//
// Move one bit at a time (!!)
// Uses shifts and masks instead of divides.
// Sets destination bit to 0/1 as source bit is 0/1.
*/
int bpv, temp, nextBit, byte, bitShift, bit;
unsigned char* source = (unsigned char*) Source;
int* destination = (int*) Destination;

    temp = 0;
    bpv = *bitsPerValue;
    nextBit = *startSkip;

    for ( bit = 0; bit < bpv; bit++ ) {
      byte     = ( nextBit ) >> 3;
      bitShift = 7 - ( nextBit++ ) & 0x7;

      temp <<= 1 ;
      if( (source[byte] & onbit[bitShift]) ) temp |= 1;
    }

    *destination = temp;
}

void GBYTE_(
  void* Source,
  void* Destination,
  int* startSkip,
  int* bitsPerValue) {
      GBYTE(Source,Destination,startSkip,bitsPerValue);
}

void GBYTE__(
  void* Source,
  void* Destination,
  int* startSkip,
  int* bitsPerValue) {
      GBYTE(Source,Destination,startSkip,bitsPerValue);
}

void SBYTES_(
  void* Destination,
  void* Source,
  int* startSkip,
  int* bitsPerValue,
  int* skipBetweenValues,
  int* numberOfValues) {
     SBYTES(Destination,Source,startSkip,bitsPerValue,skipBetweenValues,numberOfValues);
}

void SBYTES__(
  void* Destination,
  void* Source,
  int* startSkip,
  int* bitsPerValue,
  int* skipBetweenValues,
  int* numberOfValues) {
     SBYTES(Destination,Source,startSkip,bitsPerValue,skipBetweenValues,numberOfValues);
}

void SBYTES(
  void* Destination,
  void* Source,
  int* startSkip,
  int* bitsPerValue,
  int* skipBetweenValues,
  int* numberOfValues) {
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
//   0 < bitsPerValue < SWORD  Size in bits of each value
//
//   skipBetweenValues >= 0    Number of bits to be skipped between values
//
//   numberOfValues >= 0       Number of values to be packed/unpacked
//
// Move one bit at a time (!!)
// Uses shifts and masks instead of divides.
// Sets destination bit to 0/1 as source bit is 0/1.
*/
unsigned char* destination = (unsigned char*) Destination;
int* source = (int*) Source;
int bitShift, bitPosition;
int nextValueFirstBit, bpvm1;
int nextDestination, nextBit, bit;
int step, nextValue;
unsigned char setBit0[8] = {0xfe,0xfd,0xfb,0xf7,0xef,0xdf,0xbf,0x7f};
unsigned char setBit1[8] = {0x01,0x02,0x04,0x08,0x10,0x20,0x40,0x80};

    step = *bitsPerValue + *skipBetweenValues;
    nextValueFirstBit = *startSkip;
    bpvm1 = (*bitsPerValue - 1);

    for( nextValue = 0; nextValue < *numberOfValues; ++nextValue) {
      nextBit = nextValueFirstBit;

      for ( bit = 0; bit < *bitsPerValue; bit++ ) {
        nextDestination = ( nextBit ) >> 3;
        bitShift    = (bpvm1 - bit) & MASKSWORD;
        bitPosition = 7 - (( nextBit++ ) & 0x7);

        if( (source[nextValue] & onbit[bitShift]) )
          destination[nextDestination] |= setBit1[bitPosition];
        else
          destination[nextDestination] &= setBit0[bitPosition];
      }

      nextValueFirstBit += step;
    }
}

void SBYTE_(
  void* destination,
  void* source,
  int* startSkip,
  int* bitsPerValue) {
     SBYTE(destination,source,startSkip,bitsPerValue);
}

void SBYTE__(
  void* destination,
  void* source,
  int* startSkip,
  int* bitsPerValue) {
     SBYTE(destination,source,startSkip,bitsPerValue);
}

void SBYTE(
  void* destination,
  void* source,
  int* startSkip,
  int* bitsPerValue) {
/*
//  SBYTE: 
//
//   Packs one value from source to destination.
//
//   Makes an initial skip over bits in source.
//
//   startSkip >= 0            Number of bits to be skipped preceding first
//                             value in source
//
//   0 < bitsPerValue < SWORD  Size in bits of each value
//
*/
int skipBetweenValues=0, numberOfValues=1;

    SBYTES(destination,
           source,
           startSkip,
           bitsPerValue,
           &skipBetweenValues,
           &numberOfValues);
}
