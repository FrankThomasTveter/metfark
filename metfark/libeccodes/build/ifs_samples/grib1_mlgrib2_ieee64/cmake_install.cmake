# Install script for directory: /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/ifs_samples/grib1_mlgrib2_ieee64

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/metfark/metfark/metfark/libeccodes")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "RelWithDebInfo")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/eccodes/ifs_samples/grib1_mlgrib2_ieee64" TYPE FILE PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ FILES
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/ifs_samples/grib1_mlgrib2_ieee64/gg_ml.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/ifs_samples/grib1_mlgrib2_ieee64/gg_sfc.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/ifs_samples/grib1_mlgrib2_ieee64/sh_ml.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/ifs_samples/grib1_mlgrib2_ieee64/sh_sfc.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/polar_stereographic_pl_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_160_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/GRIB1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/BUFR4.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/BUFR3_local.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_sfc_jpeg_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/rotated_gg_ml_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_ml_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_sfc_jpeg_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_160_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/rotated_gg_pl_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_sfc_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_sfc_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_2000_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/regular_gg_pl_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/polar_stereographic_sfc_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_48_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_512_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_200_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_640_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_512_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_128_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/gg_sfc_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_400_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_1280_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_256_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_80_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/regular_gg_ml_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_sfc_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_320_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_96_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_256_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/sh_pl_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_512_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/rotated_ll_sfc_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_320_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_32_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_96_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/sh_pl_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_128_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_96_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/rotated_gg_sfc_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_1024_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_640_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_2000_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_400_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/regular_gg_sfc_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_200_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_1280_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/sh_ml_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/rotated_ll_pl_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_ll_sfc_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/regular_gg_ml_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_1280_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/rotated_ll_sfc_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_32_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_640_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_2000_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_48_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_2000_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_1024_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/regular_gg_pl_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_1024_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_96_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/polar_stereographic_sfc_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_128_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_32_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_80_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_80_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_640_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_64_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_160_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/regular_ll_sfc_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_48_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/BUFR3.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_400_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_ml_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/rotated_gg_ml_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/budg.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/regular_ll_pl_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_80_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/BUFR4_local.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_sfc_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/rotated_gg_pl_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/sh_sfc_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/rotated_ll_pl_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_320_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_160_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_64_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_256_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/BUFR4_local_satellite.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_1280_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_32_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_128_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_ll_sfc_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_320_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/regular_ll_pl_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_512_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/BUFR3_local_satellite.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/GRIB2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_1024_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_ml_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_256_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/sh_ml_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_48_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/sh_sfc_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/rotated_gg_sfc_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_200_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/clusters_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_ml_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/gg_sfc_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/regular_gg_sfc_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/regular_ll_sfc_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/polar_stereographic_pl_grib2.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_gg_pl_400_grib1.tmpl"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/samples/reduced_rotated_gg_pl_200_grib1.tmpl"
    )
endif()

