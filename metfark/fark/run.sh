#!/bin/csh
#
cd ../&&make&&cd fark&&cp ../*/*.mod lib&&perl Makefile.PL&&make clean&&perl Makefile.PL&&make&&sudo make install
#

