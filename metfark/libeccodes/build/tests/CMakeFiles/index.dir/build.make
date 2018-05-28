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
include tests/CMakeFiles/index.dir/depend.make

# Include the progress variables for this target.
include tests/CMakeFiles/index.dir/progress.make

# Include the compile flags for this target's objects.
include tests/CMakeFiles/index.dir/flags.make

tests/CMakeFiles/index.dir/index.c.o: tests/CMakeFiles/index.dir/flags.make
tests/CMakeFiles/index.dir/index.c.o: eccodes-2.7.0-Source/tests/index.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object tests/CMakeFiles/index.dir/index.c.o"
	cd /metfark/metfark/metfark/libeccodes/build/tests && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/index.dir/index.c.o   -c /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/index.c

tests/CMakeFiles/index.dir/index.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/index.dir/index.c.i"
	cd /metfark/metfark/metfark/libeccodes/build/tests && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/index.c > CMakeFiles/index.dir/index.c.i

tests/CMakeFiles/index.dir/index.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/index.dir/index.c.s"
	cd /metfark/metfark/metfark/libeccodes/build/tests && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests/index.c -o CMakeFiles/index.dir/index.c.s

tests/CMakeFiles/index.dir/index.c.o.requires:

.PHONY : tests/CMakeFiles/index.dir/index.c.o.requires

tests/CMakeFiles/index.dir/index.c.o.provides: tests/CMakeFiles/index.dir/index.c.o.requires
	$(MAKE) -f tests/CMakeFiles/index.dir/build.make tests/CMakeFiles/index.dir/index.c.o.provides.build
.PHONY : tests/CMakeFiles/index.dir/index.c.o.provides

tests/CMakeFiles/index.dir/index.c.o.provides.build: tests/CMakeFiles/index.dir/index.c.o


# Object files for target index
index_OBJECTS = \
"CMakeFiles/index.dir/index.c.o"

# External object files for target index
index_EXTERNAL_OBJECTS =

tests/index: tests/CMakeFiles/index.dir/index.c.o
tests/index: tests/CMakeFiles/index.dir/build.make
tests/index: lib/libeccodes.so
tests/index: /usr/lib/x86_64-linux-gnu/libm.so
tests/index: tests/CMakeFiles/index.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking C executable index"
	cd /metfark/metfark/metfark/libeccodes/build/tests && /usr/bin/cmake -E remove /metfark/metfark/metfark/libeccodes/build/tests/index
	cd /metfark/metfark/metfark/libeccodes/build/tests && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/index.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
tests/CMakeFiles/index.dir/build: tests/index

.PHONY : tests/CMakeFiles/index.dir/build

tests/CMakeFiles/index.dir/requires: tests/CMakeFiles/index.dir/index.c.o.requires

.PHONY : tests/CMakeFiles/index.dir/requires

tests/CMakeFiles/index.dir/clean:
	cd /metfark/metfark/metfark/libeccodes/build/tests && $(CMAKE_COMMAND) -P CMakeFiles/index.dir/cmake_clean.cmake
.PHONY : tests/CMakeFiles/index.dir/clean

tests/CMakeFiles/index.dir/depend:
	cd /metfark/metfark/metfark/libeccodes/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tests /metfark/metfark/metfark/libeccodes/build /metfark/metfark/metfark/libeccodes/build/tests /metfark/metfark/metfark/libeccodes/build/tests/CMakeFiles/index.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : tests/CMakeFiles/index.dir/depend
