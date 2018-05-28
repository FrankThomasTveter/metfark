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
include tests/CMakeFiles/local_MeteoFrance.dir/depend.make

# Include the progress variables for this target.
include tests/CMakeFiles/local_MeteoFrance.dir/progress.make

# Include the compile flags for this target's objects.
include tests/CMakeFiles/local_MeteoFrance.dir/flags.make

tests/CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.o: tests/CMakeFiles/local_MeteoFrance.dir/flags.make
tests/CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.o: eccodes-2.7.0-Source/tests/local_MeteoFrance.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object tests/CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.o"
	cd /metfark/metfark/metfark/libeccodes/build/tests && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.o   -c /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/local_MeteoFrance.c

tests/CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.i"
	cd /metfark/metfark/metfark/libeccodes/build/tests && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/local_MeteoFrance.c > CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.i

tests/CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.s"
	cd /metfark/metfark/metfark/libeccodes/build/tests && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/local_MeteoFrance.c -o CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.s

tests/CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.o.requires:

.PHONY : tests/CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.o.requires

tests/CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.o.provides: tests/CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.o.requires
	$(MAKE) -f tests/CMakeFiles/local_MeteoFrance.dir/build.make tests/CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.o.provides.build
.PHONY : tests/CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.o.provides

tests/CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.o.provides.build: tests/CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.o


# Object files for target local_MeteoFrance
local_MeteoFrance_OBJECTS = \
"CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.o"

# External object files for target local_MeteoFrance
local_MeteoFrance_EXTERNAL_OBJECTS =

tests/local_MeteoFrance: tests/CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.o
tests/local_MeteoFrance: tests/CMakeFiles/local_MeteoFrance.dir/build.make
tests/local_MeteoFrance: lib/libeccodes.so
tests/local_MeteoFrance: /usr/lib/x86_64-linux-gnu/libm.so
tests/local_MeteoFrance: tests/CMakeFiles/local_MeteoFrance.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking C executable local_MeteoFrance"
	cd /metfark/metfark/metfark/libeccodes/build/tests && /usr/bin/cmake -E remove /metfark/metfark/metfark/libeccodes/build/tests/local_MeteoFrance
	cd /metfark/metfark/metfark/libeccodes/build/tests && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/local_MeteoFrance.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
tests/CMakeFiles/local_MeteoFrance.dir/build: tests/local_MeteoFrance

.PHONY : tests/CMakeFiles/local_MeteoFrance.dir/build

tests/CMakeFiles/local_MeteoFrance.dir/requires: tests/CMakeFiles/local_MeteoFrance.dir/local_MeteoFrance.c.o.requires

.PHONY : tests/CMakeFiles/local_MeteoFrance.dir/requires

tests/CMakeFiles/local_MeteoFrance.dir/clean:
	cd /metfark/metfark/metfark/libeccodes/build/tests && $(CMAKE_COMMAND) -P CMakeFiles/local_MeteoFrance.dir/cmake_clean.cmake
.PHONY : tests/CMakeFiles/local_MeteoFrance.dir/clean

tests/CMakeFiles/local_MeteoFrance.dir/depend:
	cd /metfark/metfark/metfark/libeccodes/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests /metfark/metfark/metfark/libeccodes/build /metfark/metfark/metfark/libeccodes/build/tests /metfark/metfark/metfark/libeccodes/build/tests/CMakeFiles/local_MeteoFrance.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : tests/CMakeFiles/local_MeteoFrance.dir/depend
