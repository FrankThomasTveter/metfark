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

#include <stdio.h>

#include "pbio-if.h"

extern FILE** fptable;
extern int fptableSize;

FILE * pbfp(long index) {
  if( (fptable == NULL) || ((int)index < 0) || ((int)index >= fptableSize) )
    return (FILE *) NULL;
  else
    return fptable[index];
}

