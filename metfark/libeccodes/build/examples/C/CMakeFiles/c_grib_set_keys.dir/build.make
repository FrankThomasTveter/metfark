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
include examples/C/CMakeFiles/c_grib_set_keys.dir/depend.make

# Include the progress variables for this target.
include examples/C/CMakeFiles/c_grib_set_keys.dir/progress.make

# Include the compile flags for this target's objects.
include examples/C/CMakeFiles/c_grib_set_keys.dir/flags.make

examples/C/CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.o: examples/C/CMakeFiles/c_grib_set_keys.dir/flags.make
examples/C/CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.o: eccodes-2.7.0-Source/examples/C/grib_set_keys.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object examples/C/CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.o"
	cd /metfark/metfark/metfark/libeccodes/build/examples/C && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.o   -c /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/C/grib_set_keys.c

examples/C/CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.i"
	cd /metfark/metfark/metfark/libeccodes/build/examples/C && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/C/grib_set_keys.c > CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.i

examples/C/CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.s"
	cd /metfark/metfark/metfark/libeccodes/build/examples/C && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/C/grib_set_keys.c -o CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.s

examples/C/CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.o.requires:

.PHONY : examples/C/CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.o.requires

examples/C/CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.o.provides: examples/C/CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.o.requires
	$(MAKE) -f examples/C/CMakeFiles/c_grib_set_keys.dir/build.make examples/C/CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.o.provides.build
.PHONY : examples/C/CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.o.provides

examples/C/CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.o.provides.build: examples/C/CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.o


# Object files for target c_grib_set_keys
c_grib_set_keys_OBJECTS = \
"CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.o"

# External object files for target c_grib_set_keys
c_grib_set_keys_EXTERNAL_OBJECTS =

examples/C/c_grib_set_keys: examples/C/CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.o
examples/C/c_grib_set_keys: examples/C/CMakeFiles/c_grib_set_keys.dir/build.make
examples/C/c_grib_set_keys: lib/libeccodes.so
examples/C/c_grib_set_keys: /usr/lib/x86_64-linux-gnu/libm.so
examples/C/c_grib_set_keys: examples/C/CMakeFiles/c_grib_set_keys.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking C executable c_grib_set_keys"
	cd /metfark/metfark/metfark/libeccodes/build/examples/C && /usr/bin/cmake -E remove /metfark/metfark/metfark/libeccodes/build/examples/C/c_grib_set_keys
	cd /metfark/metfark/metfark/libeccodes/build/examples/C && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/c_grib_set_keys.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
examples/C/CMakeFiles/c_grib_set_keys.dir/build: examples/C/c_grib_set_keys

.PHONY : examples/C/CMakeFiles/c_grib_set_keys.dir/build

examples/C/CMakeFiles/c_grib_set_keys.dir/requires: examples/C/CMakeFiles/c_grib_set_keys.dir/grib_set_keys.c.o.requires

.PHONY : examples/C/CMakeFiles/c_grib_set_keys.dir/requires

examples/C/CMakeFiles/c_grib_set_keys.dir/clean:
	cd /metfark/metfark/metfark/libeccodes/build/examples/C && $(CMAKE_COMMAND) -P CMakeFiles/c_grib_set_keys.dir/cmake_clean.cmake
.PHONY : examples/C/CMakeFiles/c_grib_set_keys.dir/clean

examples/C/CMakeFiles/c_grib_set_keys.dir/depend:
	cd /metfark/metfark/metfark/libeccodes/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/C /metfark/metfark/metfark/libeccodes/build /metfark/metfark/metfark/libeccodes/build/examples/C /metfark/metfark/metfark/libeccodes/build/examples/C/CMakeFiles/c_grib_set_keys.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : examples/C/CMakeFiles/c_grib_set_keys.dir/depend

