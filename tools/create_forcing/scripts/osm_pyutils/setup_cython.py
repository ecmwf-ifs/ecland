# (C) Copyright 2018- ECMWF.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
# In applying this licence, ECMWF does not waive the privileges and immunities
# granted to it by virtue of its status as an intergovernmental organisation
# nor does it submit to any jurisdiction.

import sys
from distutils.core import setup
from distutils.command.build_ext import build_ext
from Cython.Build import cythonize
import numpy

# Define a new command that sets the build directory
scriptpath = None
output_directory = None
# Get the path argument
for arg in sys.argv:
    print(arg)
    if arg.startswith("--path"):
        scriptpath = arg.split("=")[1]
        if scriptpath.endswith('/'):
            scriptpath = scriptpath[:-1]  # Remove trailing / if present
        sys.argv.remove(arg)  # Remove the --path argument from sys.argv
    elif arg.startswith("--build-lib"):
        output_directory = arg.split("=")[1]

class build_ext_subclass(build_ext):
    def finalize_options(self):
        build_ext.finalize_options(self)
        # Change the build directory to your desired directory
        self.build_lib = output_directory

setup(
  name = 'cython ext',
  ext_modules = cythonize(scriptpath + "/cython_ext.pyx"),
  include_dirs=[numpy.get_include()],
  cmdclass={'build_ext': build_ext_subclass}
)
