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

# Include any dependencies generated for this target.
include examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/depend.make

# Include the progress variables for this target.
include examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/progress.make

# Include the compile flags for this target's objects.
include examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/flags.make

examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.o: examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/flags.make
examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.o: eccodes-2.7.0-Source/examples/F90/grib_multi.f90
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building Fortran object examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.o"
	cd /metfark/metfark/metfark/libeccodes/build/examples/F90 && /usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -c /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_multi.f90 -o CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.o

examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing Fortran source to CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.i"
	cd /metfark/metfark/metfark/libeccodes/build/examples/F90 && /usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -E /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_multi.f90 > CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.i

examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling Fortran source to assembly CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.s"
	cd /metfark/metfark/metfark/libeccodes/build/examples/F90 && /usr/bin/gfortran  $(Fortran_DEFINES) $(Fortran_INCLUDES) $(Fortran_FLAGS) -S /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90/grib_multi.f90 -o CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.s

examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.o.requires:

.PHONY : examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.o.requires

examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.o.provides: examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.o.requires
	$(MAKE) -f examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/build.make examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.o.provides.build
.PHONY : examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.o.provides

examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.o.provides.build: examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.o


# Object files for target eccodes_f_grib_multi
eccodes_f_grib_multi_OBJECTS = \
"CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.o"

# External object files for target eccodes_f_grib_multi
eccodes_f_grib_multi_EXTERNAL_OBJECTS =

examples/F90/eccodes_f_grib_multi: examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.o
examples/F90/eccodes_f_grib_multi: examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/build.make
examples/F90/eccodes_f_grib_multi: lib/libeccodes_f90.so
examples/F90/eccodes_f_grib_multi: lib/libeccodes.so
examples/F90/eccodes_f_grib_multi: /usr/lib/x86_64-linux-gnu/libm.so
examples/F90/eccodes_f_grib_multi: examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking Fortran executable eccodes_f_grib_multi"
	cd /metfark/metfark/metfark/libeccodes/build/examples/F90 && /usr/bin/cmake -E remove EXE_FILENAME-NOTFOUND
	cd /metfark/metfark/metfark/libeccodes/build/examples/F90 && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/eccodes_f_grib_multi.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/build: examples/F90/eccodes_f_grib_multi

.PHONY : examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/build

examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/requires: examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/grib_multi.f90.o.requires

.PHONY : examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/requires

examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/clean:
	cd /metfark/metfark/metfark/libeccodes/build/examples/F90 && $(CMAKE_COMMAND) -P CMakeFiles/eccodes_f_grib_multi.dir/cmake_clean.cmake
.PHONY : examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/clean

examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/depend:
	cd /metfark/metfark/metfark/libeccodes/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/F90 /metfark/metfark/metfark/libeccodes/build /metfark/metfark/metfark/libeccodes/build/examples/F90 /metfark/metfark/metfark/libeccodes/build/examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : examples/F90/CMakeFiles/eccodes_f_grib_multi.dir/depend

