set -ea
# For CMake

# If this environment variable is set, then become verbose
# so one can see why and how a test failed
if test "x$ECCODES_TEST_VERBOSE_OUTPUT" != "x"; then
   set -x
fi

proj_dir=/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source
data_dir=/metfark/metfark/metfark/libeccodes/build/data

# use definitions from binary dir to test if installation will be correct
def_dir="/metfark/metfark/metfark/libeccodes/build/share/eccodes/definitions"
ECCODES_DEFINITION_PATH="${def_dir}"
export ECCODES_DEFINITION_PATH

# binaries are in the TOP CMAKE_BINARY_DIR
tools_dir=/metfark/metfark/metfark/libeccodes/build/bin
tigge_dir=/metfark/metfark/metfark/libeccodes/build/bin

# If this environment variable is set, then run the
# executables with valgrind
if test "x$ECCODES_TEST_WITH_VALGRIND" != "x"; then
   tools_dir="valgrind --error-exitcode=1 -q /metfark/metfark/metfark/libeccodes/build/bin"
fi

# ecCodes tests are in the PROJECT_BINARY_DIR
test_dir=/metfark/metfark/metfark/libeccodes/build/tests

# use samples from binary dir to test if installation will be correct
samp_dir="/metfark/metfark/metfark/libeccodes/build/share/eccodes/samples"
ECCODES_SAMPLES_PATH=${samp_dir}
export ECCODES_SAMPLES_PATH

# Options
HAVE_JPEG=0
HAVE_LIBJASPER=0
HAVE_LIBOPENJPEG=0
HAVE_PNG=0
HAVE_AEC=0
