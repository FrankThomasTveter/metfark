# CMake generated Testfile for 
# Source directory: /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests
# Build directory: /metfark/metfark/metfark/libeccodes/build/tests
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(eccodes_t_definitions "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/definitions.sh")
set_tests_properties(eccodes_t_definitions PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_calendar "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/calendar.sh")
set_tests_properties(eccodes_t_calendar PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_unit_tests "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/unit_tests.sh")
set_tests_properties(eccodes_t_unit_tests PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_md5 "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/md5.sh")
set_tests_properties(eccodes_t_md5 PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_uerra "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/uerra.sh")
set_tests_properties(eccodes_t_uerra PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_2nd_order_numValues "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_2nd_order_numValues.sh")
set_tests_properties(eccodes_t_grib_2nd_order_numValues PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_julian "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/julian.sh")
set_tests_properties(eccodes_t_julian PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_ecc-359 "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_ecc-359.sh")
set_tests_properties(eccodes_t_bufr_ecc-359 PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_ecc-517 "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_ecc-517.sh")
set_tests_properties(eccodes_t_bufr_ecc-517 PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_efas "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_efas.sh")
set_tests_properties(eccodes_t_grib_efas PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_double_cmp "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_double_cmp.sh")
set_tests_properties(eccodes_t_grib_double_cmp PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_dump "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_dump.sh")
set_tests_properties(eccodes_t_bufr_dump PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_dump_decode_filter "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_dump_decode_filter.sh")
set_tests_properties(eccodes_t_bufr_dump_decode_filter PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_dump_encode_filter "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_dump_encode_filter.sh")
set_tests_properties(eccodes_t_bufr_dump_encode_filter PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufrdc_desc_ref "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufrdc_desc_ref.sh")
set_tests_properties(eccodes_t_bufrdc_desc_ref PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufrdc_ref "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufrdc_ref.sh")
set_tests_properties(eccodes_t_bufrdc_ref PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_compare "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_compare.sh")
set_tests_properties(eccodes_t_bufr_compare PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_copy "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_copy.sh")
set_tests_properties(eccodes_t_bufr_copy PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_count "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_count.sh")
set_tests_properties(eccodes_t_bufr_count PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_get "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_get.sh")
set_tests_properties(eccodes_t_bufr_get PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_filter "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_filter.sh")
set_tests_properties(eccodes_t_bufr_filter PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_filter_extract_datetime "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_filter_extract_datetime.sh")
set_tests_properties(eccodes_t_bufr_filter_extract_datetime PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_filter_extract_area "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_filter_extract_area.sh")
set_tests_properties(eccodes_t_bufr_filter_extract_area PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_json "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_json.sh")
set_tests_properties(eccodes_t_bufr_json PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_ls "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_ls.sh")
set_tests_properties(eccodes_t_bufr_ls PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_change_edition "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_change_edition.sh")
set_tests_properties(eccodes_t_bufr_change_edition PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_keys_iter "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_keys_iter.sh")
set_tests_properties(eccodes_t_bufr_keys_iter PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_get_element "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_get_element.sh")
set_tests_properties(eccodes_t_bufr_get_element PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_wmo_tables "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_wmo_tables.sh")
set_tests_properties(eccodes_t_bufr_wmo_tables PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_ecc-197 "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_ecc-197.sh")
set_tests_properties(eccodes_t_bufr_ecc-197 PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_ecc-286 "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_ecc-286.sh")
set_tests_properties(eccodes_t_bufr_ecc-286 PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_ecc-288 "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_ecc-288.sh")
set_tests_properties(eccodes_t_bufr_ecc-288 PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_ecc-313 "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_ecc-313.sh")
set_tests_properties(eccodes_t_bufr_ecc-313 PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_ecc-379 "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_ecc-379.sh")
set_tests_properties(eccodes_t_bufr_ecc-379 PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_ecc-393 "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_ecc-393.sh")
set_tests_properties(eccodes_t_bufr_ecc-393 PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_ecc-433 "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_ecc-433.sh")
set_tests_properties(eccodes_t_bufr_ecc-433 PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_ecc-490 "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_ecc-490.sh")
set_tests_properties(eccodes_t_grib_ecc-490 PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_ecc-556 "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_ecc-556.sh")
set_tests_properties(eccodes_t_bufr_ecc-556 PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_gts_get "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/gts_get.sh")
set_tests_properties(eccodes_t_gts_get PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_gts_ls "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/gts_ls.sh")
set_tests_properties(eccodes_t_gts_ls PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_gts_compare "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/gts_compare.sh")
set_tests_properties(eccodes_t_gts_compare PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_metar_ls "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/metar_ls.sh")
set_tests_properties(eccodes_t_metar_ls PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_metar_get "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/metar_get.sh")
set_tests_properties(eccodes_t_metar_get PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_metar_dump "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/metar_dump.sh")
set_tests_properties(eccodes_t_metar_dump PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_metar_compare "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/metar_compare.sh")
set_tests_properties(eccodes_t_metar_compare PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_bufr_set "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/bufr_set.sh")
set_tests_properties(eccodes_t_bufr_set PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_ieee "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/ieee.sh")
set_tests_properties(eccodes_t_ieee PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_sh_ieee64 "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/sh_ieee64.sh")
set_tests_properties(eccodes_t_sh_ieee64 PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_optimize_scaling "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/optimize_scaling.sh")
set_tests_properties(eccodes_t_optimize_scaling PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_optimize_scaling_sh "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/optimize_scaling_sh.sh")
set_tests_properties(eccodes_t_optimize_scaling_sh PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib1to2 "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib1to2.sh")
set_tests_properties(eccodes_t_grib1to2 PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib2to1 "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib2to1.sh")
set_tests_properties(eccodes_t_grib2to1 PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib1to3 "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib1to3.sh")
set_tests_properties(eccodes_t_grib1to3 PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib2to3 "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib2to3.sh")
set_tests_properties(eccodes_t_grib2to3 PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib3_templates "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib3_templates.sh")
set_tests_properties(eccodes_t_grib3_templates PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_badgrib "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/badgrib.sh")
set_tests_properties(eccodes_t_badgrib PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_ls "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_ls.sh")
set_tests_properties(eccodes_t_grib_ls PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_filter "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_filter.sh")
set_tests_properties(eccodes_t_grib_filter PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_multi "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_multi.sh")
set_tests_properties(eccodes_t_grib_multi PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_budg "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/budg.sh")
set_tests_properties(eccodes_t_budg PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_gridType "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/gridType.sh")
set_tests_properties(eccodes_t_gridType PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_octahedral "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_octahedral.sh")
set_tests_properties(eccodes_t_grib_octahedral PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_global "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_global.sh")
set_tests_properties(eccodes_t_grib_global PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_concept "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_concept.sh")
set_tests_properties(eccodes_t_grib_concept PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_decimalPrecision "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/decimalPrecision.sh")
set_tests_properties(eccodes_t_decimalPrecision PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_bitsPerValue "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_bitsPerValue.sh")
set_tests_properties(eccodes_t_grib_bitsPerValue PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_get_fail "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/get_fail.sh")
set_tests_properties(eccodes_t_get_fail PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_missing "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/missing.sh")
set_tests_properties(eccodes_t_missing PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_local "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_local.sh")
set_tests_properties(eccodes_t_grib_local PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_step "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_step.sh")
set_tests_properties(eccodes_t_grib_step PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_set "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_set.sh")
set_tests_properties(eccodes_t_grib_set PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_iterator "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_iterator.sh")
set_tests_properties(eccodes_t_grib_iterator PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_compare "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_compare.sh")
set_tests_properties(eccodes_t_grib_compare PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_level "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_level.sh")
set_tests_properties(eccodes_t_grib_level PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_index "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/index.sh")
set_tests_properties(eccodes_t_index PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_bitmap "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_bitmap.sh")
set_tests_properties(eccodes_t_grib_bitmap PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_list "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/list.sh")
set_tests_properties(eccodes_t_list PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_second_order "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_second_order.sh")
set_tests_properties(eccodes_t_grib_second_order PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_multi_from_message "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/multi_from_message.sh")
set_tests_properties(eccodes_t_multi_from_message PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_change_scanning "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_change_scanning.sh")
set_tests_properties(eccodes_t_grib_change_scanning PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_statistics "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_statistics.sh")
set_tests_properties(eccodes_t_grib_statistics PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_tigge "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/tigge.sh")
set_tests_properties(eccodes_t_tigge PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_tigge_conversions "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/tigge_conversions.sh")
set_tests_properties(eccodes_t_tigge_conversions PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_read_any "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/read_any.sh")
set_tests_properties(eccodes_t_read_any PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_dump "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_dump.sh")
set_tests_properties(eccodes_t_grib_dump PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_dump_debug "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_dump_debug.sh")
set_tests_properties(eccodes_t_grib_dump_debug PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_util_set_spec "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_util_set_spec.sh")
set_tests_properties(eccodes_t_grib_util_set_spec PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_local_MeteoFrance "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/local_MeteoFrance.sh")
set_tests_properties(eccodes_t_local_MeteoFrance PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_grib_neg_fctime "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_neg_fctime.sh")
set_tests_properties(eccodes_t_grib_neg_fctime PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_codes_split_file "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/codes_split_file.sh")
set_tests_properties(eccodes_t_codes_split_file PROPERTIES  DEPENDS "eccodes_download_gribs;eccodes_download_tigge_gribs;eccodes_download_bufrs;eccodes_download_metars;eccodes_download_gts" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
add_test(eccodes_t_lamb_az_eq_area "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/lamb_az_eq_area.sh")
set_tests_properties(eccodes_t_lamb_az_eq_area PROPERTIES  DEPENDS "eccodes_download_gribs" ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "eccodes;script")
