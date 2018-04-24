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
include tools/CMakeFiles/grib_set.dir/depend.make

# Include the progress variables for this target.
include tools/CMakeFiles/grib_set.dir/progress.make

# Include the compile flags for this target's objects.
include tools/CMakeFiles/grib_set.dir/flags.make

tools/CMakeFiles/grib_set.dir/grib_set.c.o: tools/CMakeFiles/grib_set.dir/flags.make
tools/CMakeFiles/grib_set.dir/grib_set.c.o: eccodes-2.7.0-Source/tools/grib_set.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object tools/CMakeFiles/grib_set.dir/grib_set.c.o"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/grib_set.dir/grib_set.c.o   -c /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools/grib_set.c

tools/CMakeFiles/grib_set.dir/grib_set.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/grib_set.dir/grib_set.c.i"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools/grib_set.c > CMakeFiles/grib_set.dir/grib_set.c.i

tools/CMakeFiles/grib_set.dir/grib_set.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/grib_set.dir/grib_set.c.s"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools/grib_set.c -o CMakeFiles/grib_set.dir/grib_set.c.s

tools/CMakeFiles/grib_set.dir/grib_set.c.o.requires:

.PHONY : tools/CMakeFiles/grib_set.dir/grib_set.c.o.requires

tools/CMakeFiles/grib_set.dir/grib_set.c.o.provides: tools/CMakeFiles/grib_set.dir/grib_set.c.o.requires
	$(MAKE) -f tools/CMakeFiles/grib_set.dir/build.make tools/CMakeFiles/grib_set.dir/grib_set.c.o.provides.build
.PHONY : tools/CMakeFiles/grib_set.dir/grib_set.c.o.provides

tools/CMakeFiles/grib_set.dir/grib_set.c.o.provides.build: tools/CMakeFiles/grib_set.dir/grib_set.c.o


# Object files for target grib_set
grib_set_OBJECTS = \
"CMakeFiles/grib_set.dir/grib_set.c.o"

# External object files for target grib_set
grib_set_EXTERNAL_OBJECTS =

bin/grib_set: tools/CMakeFiles/grib_set.dir/grib_set.c.o
bin/grib_set: tools/CMakeFiles/grib_set.dir/build.make
bin/grib_set: tools/libgrib_tools.a
bin/grib_set: lib/libeccodes.so
bin/grib_set: /usr/lib/x86_64-linux-gnu/libm.so
bin/grib_set: tools/CMakeFiles/grib_set.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking C executable ../bin/grib_set"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cmake -E remove /metfark/metfark/metfark/libeccodes/build/bin/grib_set
	cd /metfark/metfark/metfark/libeccodes/build/tools && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/grib_set.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
tools/CMakeFiles/grib_set.dir/build: bin/grib_set

.PHONY : tools/CMakeFiles/grib_set.dir/build

tools/CMakeFiles/grib_set.dir/requires: tools/CMakeFiles/grib_set.dir/grib_set.c.o.requires

.PHONY : tools/CMakeFiles/grib_set.dir/requires

tools/CMakeFiles/grib_set.dir/clean:
	cd /metfark/metfark/metfark/libeccodes/build/tools && $(CMAKE_COMMAND) -P CMakeFiles/grib_set.dir/cmake_clean.cmake
.PHONY : tools/CMakeFiles/grib_set.dir/clean

tools/CMakeFiles/grib_set.dir/depend:
	cd /metfark/metfark/metfark/libeccodes/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools /metfark/metfark/metfark/libeccodes/build /metfark/metfark/metfark/libeccodes/build/tools /metfark/metfark/metfark/libeccodes/build/tools/CMakeFiles/grib_set.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : tools/CMakeFiles/grib_set.dir/depend

