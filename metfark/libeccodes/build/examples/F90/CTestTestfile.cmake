# CMake generated Testfile for 
# Source directory: /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90
# Build directory: /metfark/metfark/metfark/libeccodes/build/examples/F90
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(eccodes_f_grib_index "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_index.sh")
set_tests_properties(eccodes_f_grib_index PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_copy_message "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_copy_message.sh")
set_tests_properties(eccodes_f_grib_copy_message PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_bufr_copy_message "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/bufr_copy_message.sh")
set_tests_properties(eccodes_f_bufr_copy_message PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_get_keys "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_get_keys.sh")
set_tests_properties(eccodes_f_grib_get_keys PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_get_data "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_get_data.sh")
set_tests_properties(eccodes_f_grib_get_data PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_get_pl "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/get_pl.sh")
set_tests_properties(eccodes_f_get_pl PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_get_pv "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/get_pv.sh")
set_tests_properties(eccodes_f_get_pv PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_keys_iterator "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_keys_iterator.sh")
set_tests_properties(eccodes_f_grib_keys_iterator PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_multi_write "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_multi_write.sh")
set_tests_properties(eccodes_f_grib_multi_write PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_multi "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_multi.sh")
set_tests_properties(eccodes_f_grib_multi PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_nearest "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_nearest.sh")
set_tests_properties(eccodes_f_grib_nearest PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_precision "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_precision.sh")
set_tests_properties(eccodes_f_grib_precision PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_print_data "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_print_data.sh")
set_tests_properties(eccodes_f_grib_print_data PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_set_keys "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_set_keys.sh")
set_tests_properties(eccodes_f_grib_set_keys PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_set_bitmap "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_set_bitmap.sh")
set_tests_properties(eccodes_f_grib_set_bitmap PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_set_missing "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_set_missing.sh")
set_tests_properties(eccodes_f_grib_set_missing PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_set_pv "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_set_pv.sh")
set_tests_properties(eccodes_f_grib_set_pv PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_set_data "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_set_data.sh")
set_tests_properties(eccodes_f_grib_set_data PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_samples "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_samples.sh")
set_tests_properties(eccodes_f_grib_samples PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_count_messages "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_count_messages.sh")
set_tests_properties(eccodes_f_grib_count_messages PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_count_messages_multi "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_count_messages_multi.sh")
set_tests_properties(eccodes_f_grib_count_messages_multi PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_copy_namespace "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_copy_namespace.sh")
set_tests_properties(eccodes_f_grib_copy_namespace PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_read_message "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/read_message.sh")
set_tests_properties(eccodes_f_read_message PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_read_from_file "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/read_from_file.sh")
set_tests_properties(eccodes_f_read_from_file PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_get_set_uuid "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/get_set_uuid.sh")
set_tests_properties(eccodes_f_get_set_uuid PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_grib_clone "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_clone.sh")
set_tests_properties(eccodes_f_grib_clone PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_bufr_attributes "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/bufr_attributes.sh")
set_tests_properties(eccodes_f_bufr_attributes PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_bufr_copy_data "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/bufr_copy_data.sh")
set_tests_properties(eccodes_f_bufr_copy_data PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_bufr_clone "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/bufr_clone.sh")
set_tests_properties(eccodes_f_bufr_clone PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_bufr_expanded "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/bufr_expanded.sh")
set_tests_properties(eccodes_f_bufr_expanded PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_bufr_get_keys "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/bufr_get_keys.sh")
set_tests_properties(eccodes_f_bufr_get_keys PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_bufr_get_string_array "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/bufr_get_string_array.sh")
set_tests_properties(eccodes_f_bufr_get_string_array PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_bufr_keys_iterator "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/bufr_keys_iterator.sh")
set_tests_properties(eccodes_f_bufr_keys_iterator PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_bufr_read_header "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/bufr_read_header.sh")
set_tests_properties(eccodes_f_bufr_read_header PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_bufr_read_scatterometer "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/bufr_read_scatterometer.sh")
set_tests_properties(eccodes_f_bufr_read_scatterometer PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_bufr_read_synop "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/bufr_read_synop.sh")
set_tests_properties(eccodes_f_bufr_read_synop PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_bufr_read_temp "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/bufr_read_temp.sh")
set_tests_properties(eccodes_f_bufr_read_temp PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_bufr_read_tropical_cyclone "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/bufr_read_tropical_cyclone.sh")
set_tests_properties(eccodes_f_bufr_read_tropical_cyclone PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_bufr_set_keys "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/bufr_set_keys.sh")
set_tests_properties(eccodes_f_bufr_set_keys PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_bufr_copy_keys "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/bufr_copy_keys.sh")
set_tests_properties(eccodes_f_bufr_copy_keys PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_bufr_subset "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/bufr_subset.sh")
set_tests_properties(eccodes_f_bufr_subset PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_f_get_product_kind "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/get_product_kind.sh")
set_tests_properties(eccodes_f_get_product_kind PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_bufrs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
