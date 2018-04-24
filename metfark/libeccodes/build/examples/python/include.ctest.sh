# examples/python include file for CMake

set -ea
# If this environment variable is set, then become verbose
# so one can see why and how a test failed
if test "x$ECCODES_TEST_VERBOSE_OUTPUT" != "x"; then
   set -x
fi

data_dir=/metfark/metfark/metfark/libeccodes/build/data

# use definitions from binary dir to test if installation will be correct
def_dir="/metfark/metfark/metfark/libeccodes/build/share/eccodes/definitions"
ECCODES_DEFINITION_PATH="${def_dir}"
export ECCODES_DEFINITION_PATH

tools_dir=/metfark/metfark/metfark/libeccodes/build/bin
examples_dir=/metfark/metfark/metfark/libeccodes/build/examples/python
examples_src=/metfark/metfark/metfark/libeccodes/build/eccodes-2.7.0-Source/examples/python

# use samples from binary dir to test if installation will be correct
samp_dir="/metfark/metfark/metfark/libeccodes/build/share/eccodes/samples"
ECCODES_SAMPLES_PATH=${samp_dir}
export ECCODES_SAMPLES_PATH

PYTHONPATH=/metfark/metfark/metfark/libeccodes/build/python:$PYTHONPATH
export PYTHONPATH
