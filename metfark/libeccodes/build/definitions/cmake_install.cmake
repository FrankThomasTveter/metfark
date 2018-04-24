# Install script for directory: /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions

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
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/eccodes/definitions" TYPE FILE PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ FILES
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/empty_template.def"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/boot.def"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/parameters_version.def"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/stepUnits.table"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/mars_param.table"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/param_id.table"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/CMakeLists.txt"
    )
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/eccodes/definitions" TYPE FILE FILES "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/installDefinitions.sh")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/eccodes/definitions" TYPE DIRECTORY FILES
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/budg"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/bufr"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/cdf"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/common"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/grib1"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/grib2"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/grib3"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/gts"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/mars"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/metar"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/tide"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/hdf5"
    "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/definitions/wrap"
    FILES_MATCHING REGEX "/[^/]*\\.def$" REGEX "/[^/]*\\.txt$" REGEX "/[^/]*\\.list$" REGEX "/[^/]*\\.table$" REGEX "/4\\.2\\.192\\.[^/]*\\.table$" EXCLUDE PERMISSIONS OWNER_WRITE OWNER_READ GROUP_READ WORLD_READ)
endif()

