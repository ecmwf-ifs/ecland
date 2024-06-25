
Doxygen is a documentation generation tool that automatically produces documentation from annotated source code for various programming languages (Fortran, C++, Python, etc.). 
It supports the generation of both online (HTML) and offline (PDF, LaTeX) documentation, making it easier to maintain and share code documentation.

For more info on doxygen: https://www.doxygen.nl/download.html

Installing doxygen
==================

You will need a recent doxygen version to run the `config_file` example successfully.
The version tested is 1.9.1, and the most recent one donwloaded from git.
The GIT repository for doxygen is hosted on GitHub. In this repository you can be find the latest "bleeding edge" version of doxygen.

Requirements
------------

- g++
- python
- cmake
- flex
- bison

Build doxygen
-------------

If the necessary build tools are installed (i.e. g++, python, cmake, flex, bison), 
you should do the following to build and install doxygen from the git repository:

    git clone https://github.com/doxygen/doxygen.git
    cd doxygen

Build: 

    mkdir build
    cd build
    cmake -G "Unix Makefiles" ..
    make

To force a new build after an earlier check-out simple remove the build directory and redo the steps above.
After the binaries have been built, if you are installing doxygen on a workstation you might not have permission to write on 
the default binary folder.

You can modify the path where the doxygen binaries will be installed by changing in the file `doxygen/build/cmake_install.cmake`, around ln 5:

    set(CMAKE_INSTALL_PREFIX "<set_your_local_user_path>") 

Then to install:

    make install

You can run doxygen pointing to the binary in your selected folder with the following command:

    <your_local_user_path>/doxygen doxygen/config_file

Options that you might need to modify in the doxygen `config_file`:

    INPUT
    OUTPUT_DIRECTORY
    PROJECT_NAME
    INCLUDE_PATH
    PERL_PATH
    DOT_PATH
    STRIP_FROM_PATH
