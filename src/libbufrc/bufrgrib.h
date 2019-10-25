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
#ifndef BUFRGRIB_H
#define BUFRGRIB_H
/*
  bufrgrib.h
*/
#include <stdio.h>
#include <string.h>

/*	defines for BUFR functions */ 
#define ARRSIZE 100
#define TRUE 1
#define FALSE 0
#define BOOL int
#define BOOLEAN int

#ifdef VAX
#define off_t char *
#include <types.h>
#include <file.h>
#endif

#define BUFSIZE 200
#endif /* end of  BUFRGRIB_H */
