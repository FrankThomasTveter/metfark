# CMake generated Testfile for 
# Source directory: /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/data
# Build directory: /metfark/metfark/metfark/libeccodes/build/data
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(eccodes_download_gribs "/usr/bin/cmake" "-P" "/metfark/metfark/metfark/libeccodes/build/data/get_data_eccodes_download_gribs.cmake")
set_tests_properties(eccodes_download_gribs PROPERTIES  LABELS "eccodes;download_data")
subdirs(tigge)
subdirs(bufr)
subdirs(metar)
subdirs(gts)
