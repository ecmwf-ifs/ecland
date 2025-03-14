# ifsbench-based tests

ifsbench (https://github.com/ecmwf-ifs/ifsbench) is a framework for benchmarking/testing that is used in ecLand to easily setup tests.
It is used in ecLand to easily setup new tests. A test in ecLand is composed of two components: the scientific setup (`science`) and the technical setup (`tech`).

The scientific setup describes what is tested, this includes:
* Which input files (forcing, soil, surfclim) are used
* which default namelist is used
* which modifications are applied to the namelist
* the default parallel setup

The technical setup doesn't alter the experiment setup but adds some additional technical details, for example
* additional debug namelist entries
* additional accelerator information for GPU runs
* custom environment flags for certain scenarios.

The same scientific setup can for example be used with different technical setups:
```
# Runs the my_science scientific setup with the default technical setup.
ifsbench-run.py from_yaml test.yaml my_science --tech=default

# Runs the my_science scientific setup with the debug technical setup which
# doesn't change the behaviour of the test but sets additional debug namelist
# entries.
ifsbench-run.py from_yaml test.yaml my_science --tech=debug

# Do the same but with an environment variable.
export IFSBENCH_TECH=debug
ifsbench-run.py from_yaml test.yaml my_science
```

Besides the scientific and technical setup, an architecture (`arch`) is needed to run a test. The architecture describes the underlying system and provides the default parallel run launcher (srun, mpirun, ...) and additional information about the hardware setup.

# Running the tests

There are two different ways to run the tests: By calling the ifsbench run script directly or via CTest.

## ifsbench_run.py

Building `ecLand` using CMake will install the `ifsbench_run.py` script in `build_dir/tests/ifsbench` as well as the accompanying YAML test configuration file. The YAML file specifies the available architectures, scientific and technical setups. To run the test, look up the name of the scientific setup that you want to run in the YAML file and run
```
ifsbench_run.py <path_to_yaml> <science_setup>
```
Several options can be added to alter the run
* `--run-dir <run_dir>` Specify a directory where the tests will run and which won't be cleaned up after running the test.
* `--tech <tech_name>` Use the given technical setup when running the tests. Available setups and their names can be looked up in the YAML file.
* `--arch <arch_name>` Use the given architecture when running the tests. Available architectures and their names can be looked up in the YAML file.
* `--validate <ref_path>` Check the results against for bit-identicality compared to some reference results stored in the specified file.

Instead of passing these flags, it is also possible to set  the corresponding`IFSBENCH_<FLAG_NAME>` environment variable:
```
export IFSBENCH_ARCH=atos
export IFSBENCH_TECH=debug

ifsbench_run.py <path_to_yaml> <science_setup>
```

## CTest

Tests that should be available via CTest must be added in the `CMakeLists.txt` file using `ecbuild_add_test`. After building, these tests can then be run by calling `ctest` as usual. To specify flags (like the architecture!) for all tests, you can use the environment-variable approach as shown above
```
export IFSBENCH_ARCH=atos

ctest -j 10
```

# Technical setup

## Runner script

The `ifsbench_run.py` script contains the logic of the ecLand tests:
* it describes the data pipeline
* it describes the environment variable setup
* it describes what an ecLand result is and how it should be validated
* it provides the `from_yaml` command that actually launches a test/benchmark.

The actual parameters for the tests (which input files, parallel setup, etc.) are specified in a YAML file (usually `yaml.test`) which can easily be expanded to add more tests or to modify existing ones.

## YAML configuration file

The `ifsbench_run.py` script defines an `EclandConfig` class that describes a 1-to-1 correspondence between the YAML file and the resulting Python object. So everything that is specified in the YAML, must also exist in `EclandConfig` (or one of its attributes). The mapping is automatically done by `pydantic`.

# Adding new tests

To add new tests, you have to add a new scientific setup (or technical setup or architecture) to the YAML configuration file. Please edit the YAML file in the `tests/ifsbench` *source* directory, not in the *build* directory (as this will be overwritten when CMake runs).
The parameters that you have to provide are specified in the `EclandScience` class in `ifsbench_run.py`. The most important ones should be documented in the YAML file itself. Just create a new entry in the `science` section of the YAML file, choose the right input data files and (optionally) add some namelist modifications.
Once you have adapted the YAML file, you should most likely also add it to CTest (see CTest section above).
