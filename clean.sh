#!/bin/csh
#
rm -f `find . -name \*.a`;
rm -f `find . -name \*.so`;
rm -f `find . -name \*.mod`;
rm -f `find . -name \*~`;
rm -f metfark/fark-perl*;
cd metfark && make clean;
#
