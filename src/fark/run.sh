#!/bin/csh
#
rm lib/*.a lib/*.mod;cd ../&&make&&cd fark&&cp ../libeccodes/build/lib/libeccodes*.a  ../*/*.mod lib&&perl Makefile.PL&&make clean&&perl Makefile.PL&&make&&sudo make install
#

