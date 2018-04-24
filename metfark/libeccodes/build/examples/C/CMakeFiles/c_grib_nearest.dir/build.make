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
include examples/C/CMakeFiles/c_grib_nearest.dir/depend.make

# Include the progress variables for this target.
include examples/C/CMakeFiles/c_grib_nearest.dir/progress.make

# Include the compile flags for this target's objects.
include examples/C/CMakeFiles/c_grib_nearest.dir/flags.make

examples/C/CMakeFiles/c_grib_nearest.dir/grib_nearest.c.o: examples/C/CMakeFiles/c_grib_nearest.dir/flags.make
examples/C/CMakeFiles/c_grib_nearest.dir/grib_nearest.c.o: eccodes-2.7.0-Source/examples/C/grib_nearest.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object examples/C/CMakeFiles/c_grib_nearest.dir/grib_nearest.c.o"
	cd /metfark/metfark/metfark/libeccodes/build/examples/C && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/c_grib_nearest.dir/grib_nearest.c.o   -c /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/C/grib_nearest.c

examples/C/CMakeFiles/c_grib_nearest.dir/grib_nearest.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/c_grib_nearest.dir/grib_nearest.c.i"
	cd /metfark/metfark/metfark/libeccodes/build/examples/C && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/C/grib_nearest.c > CMakeFiles/c_grib_nearest.dir/grib_nearest.c.i

examples/C/CMakeFiles/c_grib_nearest.dir/grib_nearest.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/c_grib_nearest.dir/grib_nearest.c.s"
	cd /metfark/metfark/metfark/libeccodes/build/examples/C && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/C/grib_nearest.c -o CMakeFiles/c_grib_nearest.dir/grib_nearest.c.s

examples/C/CMakeFiles/c_grib_nearest.dir/grib_nearest.c.o.requires:

.PHONY : examples/C/CMakeFiles/c_grib_nearest.dir/grib_nearest.c.o.requires

examples/C/CMakeFiles/c_grib_nearest.dir/grib_nearest.c.o.provides: examples/C/CMakeFiles/c_grib_nearest.dir/grib_nearest.c.o.requires
	$(MAKE) -f examples/C/CMakeFiles/c_grib_nearest.dir/build.make examples/C/CMakeFiles/c_grib_nearest.dir/grib_nearest.c.o.provides.build
.PHONY : examples/C/CMakeFiles/c_grib_nearest.dir/grib_nearest.c.o.provides

examples/C/CMakeFiles/c_grib_nearest.dir/grib_nearest.c.o.provides.build: examples/C/CMakeFiles/c_grib_nearest.dir/grib_nearest.c.o


# Object files for target c_grib_nearest
c_grib_nearest_OBJECTS = \
"CMakeFiles/c_grib_nearest.dir/grib_nearest.c.o"

# External object files for target c_grib_nearest
c_grib_nearest_EXTERNAL_OBJECTS =

examples/C/c_grib_nearest: examples/C/CMakeFiles/c_grib_nearest.dir/grib_nearest.c.o
examples/C/c_grib_nearest: examples/C/CMakeFiles/c_grib_nearest.dir/build.make
examples/C/c_grib_nearest: lib/libeccodes.so
examples/C/c_grib_nearest: /usr/lib/x86_64-linux-gnu/libm.so
examples/C/c_grib_nearest: examples/C/CMakeFiles/c_grib_nearest.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking C executable c_grib_nearest"
	cd /metfark/metfark/metfark/libeccodes/build/examples/C && /usr/bin/cmake -E remove /metfark/metfark/metfark/libeccodes/build/examples/C/c_grib_nearest
	cd /metfark/metfark/metfark/libeccodes/build/examples/C && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/c_grib_nearest.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
examples/C/CMakeFiles/c_grib_nearest.dir/build: examples/C/c_grib_nearest

.PHONY : examples/C/CMakeFiles/c_grib_nearest.dir/build

examples/C/CMakeFiles/c_grib_nearest.dir/requires: examples/C/CMakeFiles/c_grib_nearest.dir/grib_nearest.c.o.requires

.PHONY : examples/C/CMakeFiles/c_grib_nearest.dir/requires

examples/C/CMakeFiles/c_grib_nearest.dir/clean:
	cd /metfark/metfark/metfark/libeccodes/build/examples/C && $(CMAKE_COMMAND) -P CMakeFiles/c_grib_nearest.dir/cmake_clean.cmake
.PHONY : examples/C/CMakeFiles/c_grib_nearest.dir/clean

examples/C/CMakeFiles/c_grib_nearest.dir/depend:
	cd /metfark/metfark/metfark/libeccodes/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/C /metfark/metfark/metfark/libeccodes/build /metfark/metfark/metfark/libeccodes/build/examples/C /metfark/metfark/metfark/libeccodes/build/examples/C/CMakeFiles/c_grib_nearest.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : examples/C/CMakeFiles/c_grib_nearest.dir/depend

