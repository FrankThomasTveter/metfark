# CMake generated Testfile for 
# Source directory: /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/data/tigge
# Build directory: /metfark/metfark/metfark/libeccodes/build/data/tigge
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(eccodes_download_tigge_gribs "/usr/bin/cmake" "-P" "/metfark/metfark/metfark/libeccodes/build/data/tigge/get_data_eccodes_download_tigge_gribs.cmake")
set_tests_properties(eccodes_download_tigge_gribs PROPERTIES  LABELS "eccodes;download_data")
