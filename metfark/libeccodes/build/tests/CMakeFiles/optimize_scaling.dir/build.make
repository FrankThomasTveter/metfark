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
include tests/CMakeFiles/optimize_scaling.dir/depend.make

# Include the progress variables for this target.
include tests/CMakeFiles/optimize_scaling.dir/progress.make

# Include the compile flags for this target's objects.
include tests/CMakeFiles/optimize_scaling.dir/flags.make

tests/CMakeFiles/optimize_scaling.dir/optimize_scaling.c.o: tests/CMakeFiles/optimize_scaling.dir/flags.make
tests/CMakeFiles/optimize_scaling.dir/optimize_scaling.c.o: eccodes-2.7.0-Source/tests/optimize_scaling.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object tests/CMakeFiles/optimize_scaling.dir/optimize_scaling.c.o"
	cd /metfark/metfark/metfark/libeccodes/build/tests && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/optimize_scaling.dir/optimize_scaling.c.o   -c /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/optimize_scaling.c

tests/CMakeFiles/optimize_scaling.dir/optimize_scaling.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/optimize_scaling.dir/optimize_scaling.c.i"
	cd /metfark/metfark/metfark/libeccodes/build/tests && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/optimize_scaling.c > CMakeFiles/optimize_scaling.dir/optimize_scaling.c.i

tests/CMakeFiles/optimize_scaling.dir/optimize_scaling.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/optimize_scaling.dir/optimize_scaling.c.s"
	cd /metfark/metfark/metfark/libeccodes/build/tests && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/optimize_scaling.c -o CMakeFiles/optimize_scaling.dir/optimize_scaling.c.s

tests/CMakeFiles/optimize_scaling.dir/optimize_scaling.c.o.requires:

.PHONY : tests/CMakeFiles/optimize_scaling.dir/optimize_scaling.c.o.requires

tests/CMakeFiles/optimize_scaling.dir/optimize_scaling.c.o.provides: tests/CMakeFiles/optimize_scaling.dir/optimize_scaling.c.o.requires
	$(MAKE) -f tests/CMakeFiles/optimize_scaling.dir/build.make tests/CMakeFiles/optimize_scaling.dir/optimize_scaling.c.o.provides.build
.PHONY : tests/CMakeFiles/optimize_scaling.dir/optimize_scaling.c.o.provides

tests/CMakeFiles/optimize_scaling.dir/optimize_scaling.c.o.provides.build: tests/CMakeFiles/optimize_scaling.dir/optimize_scaling.c.o


# Object files for target optimize_scaling
optimize_scaling_OBJECTS = \
"CMakeFiles/optimize_scaling.dir/optimize_scaling.c.o"

# External object files for target optimize_scaling
optimize_scaling_EXTERNAL_OBJECTS =

tests/optimize_scaling: tests/CMakeFiles/optimize_scaling.dir/optimize_scaling.c.o
tests/optimize_scaling: tests/CMakeFiles/optimize_scaling.dir/build.make
tests/optimize_scaling: lib/libeccodes.so
tests/optimize_scaling: /usr/lib/x86_64-linux-gnu/libm.so
tests/optimize_scaling: tests/CMakeFiles/optimize_scaling.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking C executable optimize_scaling"
	cd /metfark/metfark/metfark/libeccodes/build/tests && /usr/bin/cmake -E remove /metfark/metfark/metfark/libeccodes/build/tests/optimize_scaling
	cd /metfark/metfark/metfark/libeccodes/build/tests && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/optimize_scaling.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
tests/CMakeFiles/optimize_scaling.dir/build: tests/optimize_scaling

.PHONY : tests/CMakeFiles/optimize_scaling.dir/build

tests/CMakeFiles/optimize_scaling.dir/requires: tests/CMakeFiles/optimize_scaling.dir/optimize_scaling.c.o.requires

.PHONY : tests/CMakeFiles/optimize_scaling.dir/requires

tests/CMakeFiles/optimize_scaling.dir/clean:
	cd /metfark/metfark/metfark/libeccodes/build/tests && $(CMAKE_COMMAND) -P CMakeFiles/optimize_scaling.dir/cmake_clean.cmake
.PHONY : tests/CMakeFiles/optimize_scaling.dir/clean

tests/CMakeFiles/optimize_scaling.dir/depend:
	cd /metfark/metfark/metfark/libeccodes/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests /metfark/metfark/metfark/libeccodes/build /metfark/metfark/metfark/libeccodes/build/tests /metfark/metfark/metfark/libeccodes/build/tests/CMakeFiles/optimize_scaling.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : tests/CMakeFiles/optimize_scaling.dir/depend
