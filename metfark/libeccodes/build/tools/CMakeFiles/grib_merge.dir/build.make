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
include tools/CMakeFiles/grib_merge.dir/depend.make

# Include the progress variables for this target.
include tools/CMakeFiles/grib_merge.dir/progress.make

# Include the compile flags for this target's objects.
include tools/CMakeFiles/grib_merge.dir/flags.make

tools/CMakeFiles/grib_merge.dir/grib_merge.c.o: tools/CMakeFiles/grib_merge.dir/flags.make
tools/CMakeFiles/grib_merge.dir/grib_merge.c.o: eccodes-2.7.0-Source/tools/grib_merge.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object tools/CMakeFiles/grib_merge.dir/grib_merge.c.o"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/grib_merge.dir/grib_merge.c.o   -c /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools/grib_merge.c

tools/CMakeFiles/grib_merge.dir/grib_merge.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/grib_merge.dir/grib_merge.c.i"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools/grib_merge.c > CMakeFiles/grib_merge.dir/grib_merge.c.i

tools/CMakeFiles/grib_merge.dir/grib_merge.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/grib_merge.dir/grib_merge.c.s"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools/grib_merge.c -o CMakeFiles/grib_merge.dir/grib_merge.c.s

tools/CMakeFiles/grib_merge.dir/grib_merge.c.o.requires:

.PHONY : tools/CMakeFiles/grib_merge.dir/grib_merge.c.o.requires

tools/CMakeFiles/grib_merge.dir/grib_merge.c.o.provides: tools/CMakeFiles/grib_merge.dir/grib_merge.c.o.requires
	$(MAKE) -f tools/CMakeFiles/grib_merge.dir/build.make tools/CMakeFiles/grib_merge.dir/grib_merge.c.o.provides.build
.PHONY : tools/CMakeFiles/grib_merge.dir/grib_merge.c.o.provides

tools/CMakeFiles/grib_merge.dir/grib_merge.c.o.provides.build: tools/CMakeFiles/grib_merge.dir/grib_merge.c.o


# Object files for target grib_merge
grib_merge_OBJECTS = \
"CMakeFiles/grib_merge.dir/grib_merge.c.o"

# External object files for target grib_merge
grib_merge_EXTERNAL_OBJECTS =

bin/grib_merge: tools/CMakeFiles/grib_merge.dir/grib_merge.c.o
bin/grib_merge: tools/CMakeFiles/grib_merge.dir/build.make
bin/grib_merge: tools/libgrib_tools.a
bin/grib_merge: lib/libeccodes.so
bin/grib_merge: /usr/lib/x86_64-linux-gnu/libm.so
bin/grib_merge: tools/CMakeFiles/grib_merge.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking C executable ../bin/grib_merge"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cmake -E remove /metfark/metfark/metfark/libeccodes/build/bin/grib_merge
	cd /metfark/metfark/metfark/libeccodes/build/tools && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/grib_merge.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
tools/CMakeFiles/grib_merge.dir/build: bin/grib_merge

.PHONY : tools/CMakeFiles/grib_merge.dir/build

tools/CMakeFiles/grib_merge.dir/requires: tools/CMakeFiles/grib_merge.dir/grib_merge.c.o.requires

.PHONY : tools/CMakeFiles/grib_merge.dir/requires

tools/CMakeFiles/grib_merge.dir/clean:
	cd /metfark/metfark/metfark/libeccodes/build/tools && $(CMAKE_COMMAND) -P CMakeFiles/grib_merge.dir/cmake_clean.cmake
.PHONY : tools/CMakeFiles/grib_merge.dir/clean

tools/CMakeFiles/grib_merge.dir/depend:
	cd /metfark/metfark/metfark/libeccodes/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools /metfark/metfark/metfark/libeccodes/build /metfark/metfark/metfark/libeccodes/build/tools /metfark/metfark/metfark/libeccodes/build/tools/CMakeFiles/grib_merge.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : tools/CMakeFiles/grib_merge.dir/depend

