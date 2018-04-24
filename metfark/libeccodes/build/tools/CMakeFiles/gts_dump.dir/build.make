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
include tools/CMakeFiles/gts_dump.dir/depend.make

# Include the progress variables for this target.
include tools/CMakeFiles/gts_dump.dir/progress.make

# Include the compile flags for this target's objects.
include tools/CMakeFiles/gts_dump.dir/flags.make

tools/CMakeFiles/gts_dump.dir/gts_dump.c.o: tools/CMakeFiles/gts_dump.dir/flags.make
tools/CMakeFiles/gts_dump.dir/gts_dump.c.o: eccodes-2.7.0-Source/tools/gts_dump.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object tools/CMakeFiles/gts_dump.dir/gts_dump.c.o"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/gts_dump.dir/gts_dump.c.o   -c /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools/gts_dump.c

tools/CMakeFiles/gts_dump.dir/gts_dump.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/gts_dump.dir/gts_dump.c.i"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools/gts_dump.c > CMakeFiles/gts_dump.dir/gts_dump.c.i

tools/CMakeFiles/gts_dump.dir/gts_dump.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/gts_dump.dir/gts_dump.c.s"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools/gts_dump.c -o CMakeFiles/gts_dump.dir/gts_dump.c.s

tools/CMakeFiles/gts_dump.dir/gts_dump.c.o.requires:

.PHONY : tools/CMakeFiles/gts_dump.dir/gts_dump.c.o.requires

tools/CMakeFiles/gts_dump.dir/gts_dump.c.o.provides: tools/CMakeFiles/gts_dump.dir/gts_dump.c.o.requires
	$(MAKE) -f tools/CMakeFiles/gts_dump.dir/build.make tools/CMakeFiles/gts_dump.dir/gts_dump.c.o.provides.build
.PHONY : tools/CMakeFiles/gts_dump.dir/gts_dump.c.o.provides

tools/CMakeFiles/gts_dump.dir/gts_dump.c.o.provides.build: tools/CMakeFiles/gts_dump.dir/gts_dump.c.o


# Object files for target gts_dump
gts_dump_OBJECTS = \
"CMakeFiles/gts_dump.dir/gts_dump.c.o"

# External object files for target gts_dump
gts_dump_EXTERNAL_OBJECTS =

bin/gts_dump: tools/CMakeFiles/gts_dump.dir/gts_dump.c.o
bin/gts_dump: tools/CMakeFiles/gts_dump.dir/build.make
bin/gts_dump: tools/libgrib_tools.a
bin/gts_dump: lib/libeccodes.so
bin/gts_dump: /usr/lib/x86_64-linux-gnu/libm.so
bin/gts_dump: tools/CMakeFiles/gts_dump.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking C executable ../bin/gts_dump"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cmake -E remove /metfark/metfark/metfark/libeccodes/build/bin/gts_dump
	cd /metfark/metfark/metfark/libeccodes/build/tools && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/gts_dump.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
tools/CMakeFiles/gts_dump.dir/build: bin/gts_dump

.PHONY : tools/CMakeFiles/gts_dump.dir/build

tools/CMakeFiles/gts_dump.dir/requires: tools/CMakeFiles/gts_dump.dir/gts_dump.c.o.requires

.PHONY : tools/CMakeFiles/gts_dump.dir/requires

tools/CMakeFiles/gts_dump.dir/clean:
	cd /metfark/metfark/metfark/libeccodes/build/tools && $(CMAKE_COMMAND) -P CMakeFiles/gts_dump.dir/cmake_clean.cmake
.PHONY : tools/CMakeFiles/gts_dump.dir/clean

tools/CMakeFiles/gts_dump.dir/depend:
	cd /metfark/metfark/metfark/libeccodes/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools /metfark/metfark/metfark/libeccodes/build /metfark/metfark/metfark/libeccodes/build/tools /metfark/metfark/metfark/libeccodes/build/tools/CMakeFiles/gts_dump.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : tools/CMakeFiles/gts_dump.dir/depend

