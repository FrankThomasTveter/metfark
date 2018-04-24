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
include tools/CMakeFiles/codes_split_file.dir/depend.make

# Include the progress variables for this target.
include tools/CMakeFiles/codes_split_file.dir/progress.make

# Include the compile flags for this target's objects.
include tools/CMakeFiles/codes_split_file.dir/flags.make

tools/CMakeFiles/codes_split_file.dir/codes_split_file.c.o: tools/CMakeFiles/codes_split_file.dir/flags.make
tools/CMakeFiles/codes_split_file.dir/codes_split_file.c.o: eccodes-2.7.0-Source/tools/codes_split_file.c
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building C object tools/CMakeFiles/codes_split_file.dir/codes_split_file.c.o"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -o CMakeFiles/codes_split_file.dir/codes_split_file.c.o   -c /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools/codes_split_file.c

tools/CMakeFiles/codes_split_file.dir/codes_split_file.c.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/codes_split_file.dir/codes_split_file.c.i"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -E /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools/codes_split_file.c > CMakeFiles/codes_split_file.dir/codes_split_file.c.i

tools/CMakeFiles/codes_split_file.dir/codes_split_file.c.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/codes_split_file.dir/codes_split_file.c.s"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cc  $(C_DEFINES) $(C_INCLUDES) $(C_FLAGS) -S /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools/codes_split_file.c -o CMakeFiles/codes_split_file.dir/codes_split_file.c.s

tools/CMakeFiles/codes_split_file.dir/codes_split_file.c.o.requires:

.PHONY : tools/CMakeFiles/codes_split_file.dir/codes_split_file.c.o.requires

tools/CMakeFiles/codes_split_file.dir/codes_split_file.c.o.provides: tools/CMakeFiles/codes_split_file.dir/codes_split_file.c.o.requires
	$(MAKE) -f tools/CMakeFiles/codes_split_file.dir/build.make tools/CMakeFiles/codes_split_file.dir/codes_split_file.c.o.provides.build
.PHONY : tools/CMakeFiles/codes_split_file.dir/codes_split_file.c.o.provides

tools/CMakeFiles/codes_split_file.dir/codes_split_file.c.o.provides.build: tools/CMakeFiles/codes_split_file.dir/codes_split_file.c.o


# Object files for target codes_split_file
codes_split_file_OBJECTS = \
"CMakeFiles/codes_split_file.dir/codes_split_file.c.o"

# External object files for target codes_split_file
codes_split_file_EXTERNAL_OBJECTS =

bin/codes_split_file: tools/CMakeFiles/codes_split_file.dir/codes_split_file.c.o
bin/codes_split_file: tools/CMakeFiles/codes_split_file.dir/build.make
bin/codes_split_file: tools/libgrib_tools.a
bin/codes_split_file: lib/libeccodes.so
bin/codes_split_file: /usr/lib/x86_64-linux-gnu/libm.so
bin/codes_split_file: tools/CMakeFiles/codes_split_file.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/metfark/metfark/metfark/libeccodes/build/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking C executable ../bin/codes_split_file"
	cd /metfark/metfark/metfark/libeccodes/build/tools && /usr/bin/cmake -E remove /metfark/metfark/metfark/libeccodes/build/bin/codes_split_file
	cd /metfark/metfark/metfark/libeccodes/build/tools && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/codes_split_file.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
tools/CMakeFiles/codes_split_file.dir/build: bin/codes_split_file

.PHONY : tools/CMakeFiles/codes_split_file.dir/build

tools/CMakeFiles/codes_split_file.dir/requires: tools/CMakeFiles/codes_split_file.dir/codes_split_file.c.o.requires

.PHONY : tools/CMakeFiles/codes_split_file.dir/requires

tools/CMakeFiles/codes_split_file.dir/clean:
	cd /metfark/metfark/metfark/libeccodes/build/tools && $(CMAKE_COMMAND) -P CMakeFiles/codes_split_file.dir/cmake_clean.cmake
.PHONY : tools/CMakeFiles/codes_split_file.dir/clean

tools/CMakeFiles/codes_split_file.dir/depend:
	cd /metfark/metfark/metfark/libeccodes/build && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source /metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/tools /metfark/metfark/metfark/libeccodes/build /metfark/metfark/metfark/libeccodes/build/tools /metfark/metfark/metfark/libeccodes/build/tools/CMakeFiles/codes_split_file.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : tools/CMakeFiles/codes_split_file.dir/depend

