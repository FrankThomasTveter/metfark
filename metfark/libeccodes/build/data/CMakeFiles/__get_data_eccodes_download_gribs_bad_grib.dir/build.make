# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.5

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list


# Suppress display of executed commands.
$(VERBOSE).SILENT:


# A target that is always out of date.
cmake_force:

.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /metfark/metfark/metfark/libeccodes/build

# Utility rule file for __get_data_eccodes_download_gribs_bad_grib.

# Include the progress variables for this target.
include data/CMakeFiles/__get_data_eccodes_download_gribs_bad_grib.dir/progress.make

data/CMakeFiles/__get_data_eccodes_download_gribs_bad_grib: data/bad.grib


data/bad.grib:
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --blue --bold --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "(curl) downloading http://download.ecmwf.org/test-data/eccodes/data/bad.grib"
	cd /metfark/metfark/metfark/libeccodes/build/data && /usr/bin/curl --silent --show-error --fail --output bad.grib --retry 0 --connect-timeout 30 http://download.ecmwf.org/test-data/eccodes/data/bad.grib

__get_data_eccodes_download_gribs_bad_grib: data/CMakeFiles/__get_data_eccodes_download_gribs_bad_grib
__get_data_eccodes_download_gribs_bad_grib: data/bad.grib
__get_data_eccodes_download_gribs_bad_grib: data/CMakeFiles/__get_data_eccodes_download_gribs_bad_grib.dir/build.make

.PHONY : __get_data_eccodes_download_gribs_bad_grib

# Rule to build all files generated by this target.
data/CMakeFiles/__get_data_eccodes_download_gribs_bad_grib.dir/build: __get_data_eccodes_download_gribs_bad_grib

.PHONY : data/CMakeFiles/__get_data_eccodes_download_gribs_bad_grib.dir/build

data/CMakeFiles/__get_data_eccodes_download_gribs_bad_grib.dir/clean:
	cd /metfark/metfark/metfark/libeccodes/build/data && $(CMAKE_COMMAND) -P CMakeFiles/__get_data_eccodes_download_gribs_bad_grib.dir/cmake_clean.cmake
.PHONY : data/CMakeFiles/__get_data_eccodes_download_gribs_bad_grib.dir/clean

data/CMakeFiles/__get_data_eccodes_download_gribs_bad_grib.dir/depend:
	cd /metfark/metfark/metfark/libeccodes/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/data /metfark/metfark/metfark/libeccodes/build /metfark/metfark/metfark/libeccodes/build/data /metfark/metfark/metfark/libeccodes/build/data/CMakeFiles/__get_data_eccodes_download_gribs_bad_grib.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : data/CMakeFiles/__get_data_eccodes_download_gribs_bad_grib.dir/depend

