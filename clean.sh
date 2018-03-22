#!/bin/csh
#
cp -pr /var/www/cgi-bin/* cgi-bin/;
cp -pr /var/www/html/* html/;
rm -f `find . -name \*.a`;
rm -f `find . -name \*.so`;
rm -f `find . -name \*.mod`;
rm -f `find . -name \*~`;
rm -f metfark/fark-perl*;
cd metfark && make clean;
#
