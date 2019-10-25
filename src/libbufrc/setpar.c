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
//  setpar.c
//
//  This is a replacement for the SETPAR.f FORTRAN-callable subroutine.
//  It sets the number of bits per word (kbit), 
//  and the largest negative number (kneg).
//  The diagnostic print option is no longer supported (kpar).
//
*/
#include <limits.h>
#include "fortint.h"
#include "pbio-if.h"

void setpar_(fortint * kbit, fortint * kneg, fortint * kpar)
{
    *kbit = sizeof(fortint)*8 ;
    *kneg = INTEGER_MIN ;
    return;
}

void setpar(fortint * kbit, fortint * kneg, fortint * kpar) {

  setpar_(kbit,kneg,kpar);
}

void setpar__(fortint * kbit, fortint * kneg, fortint * kpar) {

  setpar_(kbit,kneg,kpar);
}
