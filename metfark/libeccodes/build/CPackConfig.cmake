# This file will be configured to contain variables for CPack. These variables
# should be set in the CMake list file of the project before CPack module is
# included. The list of available CPACK_xxx variables and their associated
# documentation may be obtained using
#  cpack --help-variable-list
#
# Some variables are common to all generators (e.g. CPACK_PACKAGE_NAME)
# and some are specific to a generator
# (e.g. CPACK_NSIS_EXTRA_INSTALL_COMMANDS). The generator specific variables
# usually begin with CPACK_<GENNAME>_xxxx.


SET(CPACK_BINARY_7Z "")
SET(CPACK_BINARY_BUNDLE "")
SET(CPACK_BINARY_CYGWIN "")
SET(CPACK_BINARY_DEB "")
SET(CPACK_BINARY_DRAGNDROP "")
SET(CPACK_BINARY_IFW "")
SET(CPACK_BINARY_NSIS "")
SET(CPACK_BINARY_OSXX11 "")
SET(CPACK_BINARY_PACKAGEMAKER "")
SET(CPACK_BINARY_RPM "")
SET(CPACK_BINARY_STGZ "")
SET(CPACK_BINARY_TBZ2 "")
SET(CPACK_BINARY_TGZ "")
SET(CPACK_BINARY_TXZ "")
SET(CPACK_BINARY_TZ "")
SET(CPACK_BINARY_WIX "")
SET(CPACK_BINARY_ZIP "")
SET(CPACK_CMAKE_GENERATOR "Unix Makefiles")
SET(CPACK_COMPONENTS_ALL "Unspecified;utilities")
SET(CPACK_COMPONENT_UNSPECIFIED_HIDDEN "TRUE")
SET(CPACK_COMPONENT_UNSPECIFIED_REQUIRED "TRUE")
SET(CPACK_GENERATOR "TGZ")
SET(CPACK_INSTALL_CMAKE_PROJECTS "/metfark/metfark/metfark/libeccodes/build;eccodes;ALL;/")
SET(CPACK_INSTALL_PREFIX "/metfark/metfark/metfark/libeccodes")
SET(CPACK_MODULE_PATH "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/cmake;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/../ecbuild/cmake;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/cmake/contrib")
SET(CPACK_NSIS_DISPLAY_NAME "eccodes 2.7.0")
SET(CPACK_NSIS_INSTALLER_ICON_CODE "")
SET(CPACK_NSIS_INSTALLER_MUI_ICON_CODE "")
SET(CPACK_NSIS_INSTALL_ROOT "$PROGRAMFILES")
SET(CPACK_NSIS_PACKAGE_NAME "eccodes 2.7.0")
SET(CPACK_OUTPUT_CONFIG_FILE "/metfark/metfark/metfark/libeccodes/build/CPackConfig.cmake")
SET(CPACK_PACKAGE_DEFAULT_LOCATION "/")
SET(CPACK_PACKAGE_DESCRIPTION_FILE "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/INSTALL")
SET(CPACK_PACKAGE_DESCRIPTION_SUMMARY "eccodes misses a description")
SET(CPACK_PACKAGE_FILE_NAME "eccodes-2.7.0-Linux-x86_64")
SET(CPACK_PACKAGE_INSTALL_DIRECTORY "eccodes 2.7.0")
SET(CPACK_PACKAGE_INSTALL_REGISTRY_KEY "eccodes 2.7.0")
SET(CPACK_PACKAGE_NAME "eccodes")
SET(CPACK_PACKAGE_RELOCATABLE "true")
SET(CPACK_PACKAGE_VENDOR "ECMWF")
SET(CPACK_PACKAGE_VERSION "2.7.0")
SET(CPACK_PACKAGE_VERSION_MAJOR "0")
SET(CPACK_PACKAGE_VERSION_MINOR "1")
SET(CPACK_PACKAGE_VERSION_PATCH "1")
SET(CPACK_RESOURCE_FILE_LICENSE "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/LICENSE")
SET(CPACK_RESOURCE_FILE_README "/usr/share/cmake-3.5/Templates/CPack.GenericDescription.txt")
SET(CPACK_RESOURCE_FILE_WELCOME "/usr/share/cmake-3.5/Templates/CPack.GenericWelcome.txt")
SET(CPACK_SET_DESTDIR "OFF")
SET(CPACK_SOURCE_7Z "")
SET(CPACK_SOURCE_CYGWIN "")
SET(CPACK_SOURCE_GENERATOR "TGZ")
SET(CPACK_SOURCE_IGNORE_FILES "/build/;/\\.git/;/\\.svn/;CMakeLists.txt.user;\\.swp$;p4config;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/concepts/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests.ecmwf/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/doxygen/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/confluence/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples.dev/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/templates/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/parameters/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/java/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/perl/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/config/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/m4/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/rpms/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/gaussian_experimental/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/gribex/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F77/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/extra/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/deprecated/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/bamboo/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/fortran/fortranCtypes/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tigge/tools/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/share/eccodes/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/.settings/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/concepts/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests.ecmwf/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/doxygen/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/confluence/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples.dev/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/templates/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/parameters/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/java/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/perl/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/config/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/m4/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/rpms/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/gaussian_experimental/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/gribex/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F77/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/extra/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/deprecated/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/bamboo/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/fortran/fortranCtypes/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tigge/tools/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/share/eccodes/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/.settings/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/concepts/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests.ecmwf/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/doxygen/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/confluence/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples.dev/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/templates/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/parameters/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/java/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/perl/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/config/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/m4/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/rpms/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/gaussian_experimental/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/gribex/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F77/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/extra/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/deprecated/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/bamboo/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/fortran/fortranCtypes/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tigge/tools/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/share/eccodes/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/.settings/")
SET(CPACK_SOURCE_INSTALLED_DIRECTORIES "/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source;.;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/cmake;cmake/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/share/ecbuild/toolchains;share/ecbuild/toolchains/;/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/bin;bin/")
SET(CPACK_SOURCE_OUTPUT_CONFIG_FILE "/metfark/metfark/metfark/libeccodes/build/CPackSourceConfig.cmake")
SET(CPACK_SOURCE_TBZ2 "")
SET(CPACK_SOURCE_TGZ "")
SET(CPACK_SOURCE_TXZ "")
SET(CPACK_SOURCE_TZ "")
SET(CPACK_SOURCE_ZIP "")
SET(CPACK_SYSTEM_NAME "Linux")
SET(CPACK_TOPLEVEL_TAG "Linux")
SET(CPACK_WIX_SIZEOF_VOID_P "8")

if(NOT CPACK_PROPERTIES_FILE)
  set(CPACK_PROPERTIES_FILE "/metfark/metfark/metfark/libeccodes/build/CPackProperties.cmake")
endif()

if(EXISTS ${CPACK_PROPERTIES_FILE})
  include(${CPACK_PROPERTIES_FILE})
endif()
