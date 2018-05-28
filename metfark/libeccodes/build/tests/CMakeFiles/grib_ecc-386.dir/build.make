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
include tests/CMakeFiles/grib_ecc-386.dir/depend.make

# Include the progress variables for this target.
include tests/CMakeFiles/grib_ecc-386.dir/progress.make

# Include the compile flags for this target's objects.
include tests/CMakeFiles/grib_ecc-386.dir/flags.make

tests/CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.o: tests/CMakeFiles/grib_ecc-386.dir/flags.make
tests/CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.o: eccodes-2.7.0-Source/tests/grib_ecc-386.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object tests/CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.o"
	cd /metfark/metfark/metfark/libeccodes/build/tests && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.o   -c /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_ecc-386.c

tests/CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.i"
	cd /metfark/metfark/metfark/libeccodes/build/tests && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_ecc-386.c > CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.i

tests/CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.s"
	cd /metfark/metfark/metfark/libeccodes/build/tests && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/grib_ecc-386.c -o CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.s

tests/CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.o.requires:

.PHONY : tests/CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.o.requires

tests/CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.o.provides: tests/CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.o.requires
	$(MAKE) -f tests/CMakeFiles/grib_ecc-386.dir/build.make tests/CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.o.provides.build
.PHONY : tests/CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.o.provides

tests/CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.o.provides.build: tests/CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.o


# Object files for target grib_ecc-386
grib_ecc__386_OBJECTS = \
"CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.o"

# External object files for target grib_ecc-386
grib_ecc__386_EXTERNAL_OBJECTS =

tests/grib_ecc-386: tests/CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.o
tests/grib_ecc-386: tests/CMakeFiles/grib_ecc-386.dir/build.make
tests/grib_ecc-386: lib/libeccodes.so
tests/grib_ecc-386: /usr/lib/x86_64-linux-gnu/libm.so
tests/grib_ecc-386: tests/CMakeFiles/grib_ecc-386.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking C executable grib_ecc-386"
	cd /metfark/metfark/metfark/libeccodes/build/tests && /usr/bin/cmake -E remove /metfark/metfark/metfark/libeccodes/build/tests/grib_ecc-386
	cd /metfark/metfark/metfark/libeccodes/build/tests && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/grib_ecc-386.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
tests/CMakeFiles/grib_ecc-386.dir/build: tests/grib_ecc-386

.PHONY : tests/CMakeFiles/grib_ecc-386.dir/build

tests/CMakeFiles/grib_ecc-386.dir/requires: tests/CMakeFiles/grib_ecc-386.dir/grib_ecc-386.c.o.requires

.PHONY : tests/CMakeFiles/grib_ecc-386.dir/requires

tests/CMakeFiles/grib_ecc-386.dir/clean:
	cd /metfark/metfark/metfark/libeccodes/build/tests && $(CMAKE_COMMAND) -P CMakeFiles/grib_ecc-386.dir/cmake_clean.cmake
.PHONY : tests/CMakeFiles/grib_ecc-386.dir/clean

tests/CMakeFiles/grib_ecc-386.dir/depend:
	cd /metfark/metfark/metfark/libeccodes/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests /metfark/metfark/metfark/libeccodes/build /metfark/metfark/metfark/libeccodes/build/tests /metfark/metfark/metfark/libeccodes/build/tests/CMakeFiles/grib_ecc-386.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : tests/CMakeFiles/grib_ecc-386.dir/depend
