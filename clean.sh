#!/bin/csh
#
rm -f `find . -name \*.a`;
rm -f `find . -name \*.so`;
rm -f `find . -name \*.mod`;
rm -f `find . -name \*~`;
cd metfark && make clean;
#
