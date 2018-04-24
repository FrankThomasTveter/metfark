# CMake generated Testfile for 
# Source directory: /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/data/bufr
# Build directory: /metfark/metfark/metfark/libeccodes/build/data/bufr
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(eccodes_download_bufrs "/usr/bin/cmake" "-P" "/metfark/metfark/metfark/libeccodes/build/data/bufr/get_data_eccodes_download_bufrs.cmake")
set_tests_properties(eccodes_download_bufrs PROPERTIES  LABELS "eccodes;download_data")
