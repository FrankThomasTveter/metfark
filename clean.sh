#!/bin/csh
#
rm -f `find . -name \*.a`;
rm -f `find . -name \*.so`;
rm -f `find . -name \*.mod`;
rm -f `find . -name \*~`;
rm -f src/fark-perl*;
cd src && make clean;
#
