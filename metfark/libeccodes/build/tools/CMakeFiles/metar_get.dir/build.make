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
include tools/CMakeFiles/metar_get.dir/depend.make

# Include the progress variables for this target.
include tools/CMakeFiles/metar_get.dir/progress.make

# Include the compile flags for this target's objects.
include tools/CMakeFiles/metar_get.dir/flags.make

tools/CMakeFiles/metar_get.dir/metar_get.c.o: tools/CMakeFiles/metar_get.dir/flags.make
tools/CMakeFiles/metar_get.dir/metar_get.c.o: eccodes-2.7.0-Source/tools/metar_get.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object tools/CMakeFiles/metar_get.dir/metar_get.c.o"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/metar_get.dir/metar_get.c.o   -c /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools/metar_get.c

tools/CMakeFiles/metar_get.dir/metar_get.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/metar_get.dir/metar_get.c.i"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools/metar_get.c > CMakeFiles/metar_get.dir/metar_get.c.i

tools/CMakeFiles/metar_get.dir/metar_get.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/metar_get.dir/metar_get.c.s"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools/metar_get.c -o CMakeFiles/metar_get.dir/metar_get.c.s

tools/CMakeFiles/metar_get.dir/metar_get.c.o.requires:

.PHONY : tools/CMakeFiles/metar_get.dir/metar_get.c.o.requires

tools/CMakeFiles/metar_get.dir/metar_get.c.o.provides: tools/CMakeFiles/metar_get.dir/metar_get.c.o.requires
	$(MAKE) -f tools/CMakeFiles/metar_get.dir/build.make tools/CMakeFiles/metar_get.dir/metar_get.c.o.provides.build
.PHONY : tools/CMakeFiles/metar_get.dir/metar_get.c.o.provides

tools/CMakeFiles/metar_get.dir/metar_get.c.o.provides.build: tools/CMakeFiles/metar_get.dir/metar_get.c.o


# Object files for target metar_get
metar_get_OBJECTS = \
"CMakeFiles/metar_get.dir/metar_get.c.o"

# External object files for target metar_get
metar_get_EXTERNAL_OBJECTS =

bin/metar_get: tools/CMakeFiles/metar_get.dir/metar_get.c.o
bin/metar_get: tools/CMakeFiles/metar_get.dir/build.make
bin/metar_get: tools/libgrib_tools.a
bin/metar_get: lib/libeccodes.so
bin/metar_get: /usr/lib/x86_64-linux-gnu/libm.so
bin/metar_get: tools/CMakeFiles/metar_get.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking C executable ../bin/metar_get"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cmake -E remove /metfark/metfark/metfark/libeccodes/build/bin/metar_get
	cd /metfark/metfark/metfark/libeccodes/build/tools && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/metar_get.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
tools/CMakeFiles/metar_get.dir/build: bin/metar_get

.PHONY : tools/CMakeFiles/metar_get.dir/build

tools/CMakeFiles/metar_get.dir/requires: tools/CMakeFiles/metar_get.dir/metar_get.c.o.requires

.PHONY : tools/CMakeFiles/metar_get.dir/requires

tools/CMakeFiles/metar_get.dir/clean:
	cd /metfark/metfark/metfark/libeccodes/build/tools && $(CMAKE_COMMAND) -P CMakeFiles/metar_get.dir/cmake_clean.cmake
.PHONY : tools/CMakeFiles/metar_get.dir/clean

tools/CMakeFiles/metar_get.dir/depend:
	cd /metfark/metfark/metfark/libeccodes/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools /metfark/metfark/metfark/libeccodes/build /metfark/metfark/metfark/libeccodes/build/tools /metfark/metfark/metfark/libeccodes/build/tools/CMakeFiles/metar_get.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : tools/CMakeFiles/metar_get.dir/depend

