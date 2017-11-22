#!/bin/csh
#
cd ../&&make&&cd fark&&cp ../*/*.mod lib&&make clean&&perl Makefile.PL&&make&&sudo make install
#

