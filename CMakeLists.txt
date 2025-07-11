# To build the CMake-based bits you first need to set up the build directory
# (out of tree builds are preferred). For that run:
#
#    mkdir build/ && cd build/ && cmake ..
#
# And then under the build/ directory simply call
#
#    make
#
# which will compile and install all the libraries to lib/
#

# Updated minimum CMake requirement to avoid deprecation warnings
# The minimum version 3.10 ensures good FindBLAS module support and
# modern CMake features for OpenMP detection
cmake_minimum_required(VERSION 3.10)

project(grasp LANGUAGES Fortran)

enable_language(Fortran)
enable_testing()

# "Release" will be the default build type, which gives us optimization flags etc.
# The other relevant option would be "Debug", which disables optimizations and
# enables debugging symbols. The debug build can be enabled when setting up the
# build directory with CMake:
#
#     cmake -DCMAKE_BUILD_TYPE=Debug ..
#
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE "Release" CACHE STRING
        "Choose the type of build, options are: Release Debug."
        FORCE
    )
endif(NOT CMAKE_BUILD_TYPE)

# Find the LAPACK and BLAS libraries
find_package(BLAS REQUIRED)
find_package(LAPACK REQUIRED)

# Find OpenMP
find_package(OpenMP)
if(OpenMP_Fortran_FOUND)
    message("OpenMP support found and enabled")
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
    set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${OpenMP_EXE_LINKER_FLAGS}")
else()
    message("OpenMP not found - building serial version")
    # Add -fno-automatic for serial builds to avoid conflicts
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fno-automatic")
endif()

# We need special functions to handle linking Fortran modules between libraries
# etc. The Fortran_MODULE_DIRECTORY_root variable is the directory where all the
# .mod files get written to. It is set be the modules/ subdirectory of the build
# directory.
#
# For every library, the modules get stored in
# ${Fortran_MODULE_DIRECTORY_root}/<library_name>/ so the modules from different
# libraries are separated from each other.
#
#
# Command: setup_fortran_modules(target)
#
# Needs to be called on all libraries that provide modules. It set the
# Fortran_MODULE_DIRECTORY variable for the target, which is then used by
# target_link_libraries_Fortran to set up the appropriate include directories.
#
# Example:
#
#     setup_fortran_modules(9290)
#
#
# Command: target_link_libraries_Fortran(target mode libraries...)
#
# Similar to target_link_libraries(), but will also set up paths so that the
# compiler could fine the the Fortran .mod files from of the libraries. Unlike
# for the standard command, mode ( = PUBLIC, PRIVATE) is mandatory.
#
# Modified version of: https://stackoverflow.com/a/43918277/1601695
#
# Example:
#
#     target_link_libraries_Fortran(rcsfsplit PRIVATE mod 9290)
#
set(Fortran_MODULE_DIRECTORY_root ${CMAKE_CURRENT_BINARY_DIR}/modules)
function(setup_fortran_modules target)
    set_property(TARGET ${target} PROPERTY Fortran_MODULE_DIRECTORY "${Fortran_MODULE_DIRECTORY_root}/${target}")
    install(DIRECTORY "${Fortran_MODULE_DIRECTORY_root}/${target}" DESTINATION "${CMAKE_INSTALL_PREFIX}/lib/"
        FILES_MATCHING PATTERN "*.mod")
endfunction()
function(target_link_libraries_Fortran target mode)
    target_link_libraries(${target} ${mode} ${ARGN})
    foreach(lib IN LISTS ARGN)
        target_include_directories(${target} ${mode} $<TARGET_PROPERTY:${lib},Fortran_MODULE_DIRECTORY>)
    endforeach()
endfunction()


# We put the compiled binaries into the bin/ subdirectory of the build directory
# and libraries into the lib/ subdirectory.
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/bin/")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/lib/")
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}/lib/")
# To install the binaries into the standard <repo>/bin/ directory, you need to
# call `make install`.
set(CMAKE_INSTALL_PREFIX ${PROJECT_SOURCE_DIR})

# Additional Fortran compiler flags.
#
# -fallow-argument-mismatch: needed for compatibility with modern gfortran
# -fno-automatic: only for serial builds (conflicts with OpenMP's -frecursive)
#
# Note: optimization should be enabled on the Release target automatically.
# Note: OpenMP flags and -fno-automatic are set above based on OpenMP availability
#
# If need be, you can also set up linker flags. E.g.:
#
#     set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static-libgfortran")
#
set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch -fallow-invalid-boz")

message("Compiler flags etc. for this GRASP build:")
message("* CMAKE_BUILD_TYPE:                ${CMAKE_BUILD_TYPE}")
message("* CMAKE_Fortran_COMPILER:          ${CMAKE_Fortran_COMPILER}")
message("* CMAKE_Fortran_COMPILER_VERSION:  ${CMAKE_Fortran_COMPILER_VERSION}")
message("* CMAKE_Fortran_FLAGS:             ${CMAKE_Fortran_FLAGS}")
message("* CMAKE_Fortran_FLAGS_RELEASE:     ${CMAKE_Fortran_FLAGS_RELEASE}")
message("* CMAKE_Fortran_FLAGS_DEBUG:       ${CMAKE_Fortran_FLAGS_DEBUG}")
message("* CMAKE_EXE_LINKER_FLAGS:          ${CMAKE_EXE_LINKER_FLAGS}")
message("* CMAKE_STATIC_LINKER_FLAGS:       ${CMAKE_STATIC_LINKER_FLAGS}")
message("* CMAKE_RUNTIME_OUTPUT_DIRECTORY:  ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}")
message("* CMAKE_LIBRARY_OUTPUT_DIRECTORY:  ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}")
message("* CMAKE_ARCHIVE_OUTPUT_DIRECTORY:  ${CMAKE_ARCHIVE_OUTPUT_DIRECTORY}")
message("* CMAKE_POSITION_INDEPENDENT_CODE: ${CMAKE_POSITION_INDEPENDENT_CODE}")
message("* BLAS_LIBRARIES:                  ${BLAS_LIBRARIES}")
message("* BLAS_LINKER_FLAGS:               ${BLAS_LINKER_FLAGS}")
message("* LAPACK_LIBRARIES:                ${LAPACK_LIBRARIES}")
message("* LAPACK_LINKER_FLAGS:             ${LAPACK_LINKER_FLAGS}")
if(OpenMP_Fortran_FOUND)
    message("* OpenMP_Fortran_FLAGS:            ${OpenMP_Fortran_FLAGS}")
    message("* OpenMP_EXE_LINKER_FLAGS:         ${OpenMP_EXE_LINKER_FLAGS}")
endif()

# GRASP libraries
add_subdirectory("src/lib/libmod")
add_subdirectory("src/lib/lib9290")
add_subdirectory("src/lib/libdvd90")
add_subdirectory("src/lib/libmcp90")
add_subdirectory("src/lib/librang90")

# RHFS90 application program (our main target)
add_subdirectory("src/rhfs90")

# RHFSZEEMAN95 application program (hyperfine and Zeeman structure)
add_subdirectory("src/rhfszeeman95")

# RIS4 application program (isotope shift calculations)
add_subdirectory("src/ris4")

# We only build MPI programs and libraries if we can actually find MPI on
# the user's system.
find_package(MPI)
if(MPI_Fortran_FOUND)
    message("* MPI_Fortran_LIBRARIES:          ${MPI_Fortran_LIBRARIES}")
    message("* MPI_Fortran_INCLUDE_PATH:       ${MPI_Fortran_INCLUDE_PATH}")
    message("* MPI_Fortran_COMPILE_FLAGS:      ${MPI_Fortran_COMPILE_FLAGS}")
    message("* MPI_Fortran_LINK_FLAGS:         ${MPI_Fortran_LINK_FLAGS}")

    add_subdirectory("src/lib/mpi90")
else()
    message("MPI libraries not found. Not building MPI-dependent programs.")
endif(MPI_Fortran_FOUND)

# We use the CMakeLists.txt so that the user could easily add additional targets
# to the GRASP build, such as additional (external) GRASP programs that need to
# be linked agains the GRASP libraries.
#
# We also set the GRASP variable that the user-defined targets can use to figure
# out where the root of the GRASP source tree is.
set(GRASP ${PROJECT_SOURCE_DIR})
unset(GRASP_CMakeLists_user CACHE)
find_file(GRASP_CMakeLists_user "CMakeLists.user" ${PROJECT_SOURCE_DIR})
if(NOT "${GRASP_CMakeLists_user}" STREQUAL "GRASP_CMakeLists_user-NOTFOUND")
    include(${GRASP_CMakeLists_user})
endif()
