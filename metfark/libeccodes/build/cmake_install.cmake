# Install script for directory: /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source

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
  EXECUTE_PROCESS (COMMAND "/usr/bin/cmake" -E copy_directory "/metfark/metfark/metfark/libeccodes/build/fortran/modules/${BUILD_TYPE}" "include")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE FILE FILES "/metfark/metfark/metfark/libeccodes/build/eccodes_ecbuild_config.h")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/include" TYPE FILE FILES "/metfark/metfark/metfark/libeccodes/build/eccodes_config.h")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "utilities")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/pkgconfig" TYPE FILE FILES "/metfark/metfark/metfark/libeccodes/build/eccodes.pc")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "utilities")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/pkgconfig" TYPE FILE FILES "/metfark/metfark/metfark/libeccodes/build/eccodes_f90.pc")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/eccodes/cmake" TYPE FILE FILES "/metfark/metfark/metfark/libeccodes/build/eccodes-config-version.cmake")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/eccodes/cmake" TYPE FILE FILES "/metfark/metfark/metfark/libeccodes/build/eccodes-import.cmake")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/eccodes/cmake" TYPE FILE FILES "/metfark/metfark/metfark/libeccodes/build/CMakeFiles/eccodes-config.cmake")
endif()

if(NOT CMAKE_INSTALL_COMPONENT OR "${CMAKE_INSTALL_COMPONENT}" STREQUAL "Unspecified")
  if(EXISTS "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/share/eccodes/cmake/eccodes-targets.cmake")
    file(DIFFERENT EXPORT_FILE_CHANGED FILES
         "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/share/eccodes/cmake/eccodes-targets.cmake"
         "/metfark/metfark/metfark/libeccodes/build/CMakeFiles/Export/share/eccodes/cmake/eccodes-targets.cmake")
    if(EXPORT_FILE_CHANGED)
      file(GLOB OLD_CONFIG_FILES "$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/share/eccodes/cmake/eccodes-targets-*.cmake")
      if(OLD_CONFIG_FILES)
        message(STATUS "Old export file \"$ENV{DESTDIR}${CMAKE_INSTALL_PREFIX}/share/eccodes/cmake/eccodes-targets.cmake\" will be replaced.  Removing files [${OLD_CONFIG_FILES}].")
        file(REMOVE ${OLD_CONFIG_FILES})
      endif()
    endif()
  endif()
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/eccodes/cmake" TYPE FILE FILES "/metfark/metfark/metfark/libeccodes/build/CMakeFiles/Export/share/eccodes/cmake/eccodes-targets.cmake")
  if("${CMAKE_INSTALL_CONFIG_NAME}" MATCHES "^([Rr][Ee][Ll][Ww][Ii][Tt][Hh][Dd][Ee][Bb][Ii][Nn][Ff][Oo])$")
    file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/share/eccodes/cmake" TYPE FILE FILES "/metfark/metfark/metfark/libeccodes/build/CMakeFiles/Export/share/eccodes/cmake/eccodes-targets-relwithdebinfo.cmake")
  endif()
endif()

if(NOT CMAKE_INSTALL_LOCAL_ONLY)
  # Include the install script for each subdirectory.
  include("/metfark/metfark/metfark/libeccodes/build/definitions/cmake_install.cmake")
  include("/metfark/metfark/metfark/libeccodes/build/memfs/cmake_install.cmake")
  include("/metfark/metfark/metfark/libeccodes/build/src/cmake_install.cmake")
  include("/metfark/metfark/metfark/libeccodes/build/tools/cmake_install.cmake")
  include("/metfark/metfark/metfark/libeccodes/build/fortran/cmake_install.cmake")
  include("/metfark/metfark/metfark/libeccodes/build/python/cmake_install.cmake")
  include("/metfark/metfark/metfark/libeccodes/build/tests/cmake_install.cmake")
  include("/metfark/metfark/metfark/libeccodes/build/tigge/cmake_install.cmake")
  include("/metfark/metfark/metfark/libeccodes/build/examples/cmake_install.cmake")
  include("/metfark/metfark/metfark/libeccodes/build/data/cmake_install.cmake")
  include("/metfark/metfark/metfark/libeccodes/build/samples/cmake_install.cmake")
  include("/metfark/metfark/metfark/libeccodes/build/ifs_samples/cmake_install.cmake")

endif()

if(CMAKE_INSTALL_COMPONENT)
  set(CMAKE_INSTALL_MANIFEST "install_manifest_${CMAKE_INSTALL_COMPONENT}.txt")
else()
  set(CMAKE_INSTALL_MANIFEST "install_manifest.txt")
endif()

string(REPLACE ";" "\n" CMAKE_INSTALL_MANIFEST_CONTENT
       "${CMAKE_INSTALL_MANIFEST_FILES}")
file(WRITE "/metfark/metfark/metfark/libeccodes/build/${CMAKE_INSTALL_MANIFEST}"
     "${CMAKE_INSTALL_MANIFEST_CONTENT}")
