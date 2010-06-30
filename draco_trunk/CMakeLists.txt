##
##  DRACO ADA COMPILER
##  CMAKE BUILD SCRIPT - PRIMARY
##
##
##  Copyright (c) 2010, John Marino (www.auroraux.org)
##  All rights reserved.
##
##  Permission to use, copy, modify, and/or distribute this software for any
##  purpose with or without fee is hereby granted, provided that the above
##  copyright notice and this permission notice appear in all copies.
##
##  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
##  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
##  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
##  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
##  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
##  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
##  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
##

cmake_minimum_required ( VERSION 2.6 )
project ( DRACO C )


# Set constants
set (DRACO_VERSION "2010-Q3")



# Set directory variables
set (DIR_STAGE_1    ${CMAKE_BINARY_DIR}/stage_1)
set (DIR_STAGE_2    ${CMAKE_BINARY_DIR}/stage_2)
set (DIR_STAGE_3    ${CMAKE_BINARY_DIR}/stage_3)
set (DIR_C_OBJS     ${CMAKE_BINARY_DIR}/c_objs)
set (DIR_C_XENO     ${CMAKE_BINARY_DIR}/c_xeno)
set (DIR_CONSTRUCTS ${CMAKE_SOURCE_DIR}/constructs)
set (DIR_REMNANTS   ${CMAKE_SOURCE_DIR}/gcc_remnants)



# prevent in-source builds for the user's own good
if ("${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
   message (FATAL_ERROR "Attempted in-source build.  Please run cmake from the build directory.")
endif ()



# Retrieve the HOST machine triplet information
include (${DIR_CONSTRUCTS}/cm_triplets.txt)
include (${DIR_CONSTRUCTS}/cm_host_specific.txt)



# set up build directories
file (MAKE_DIRECTORY
      ${DIR_STAGE_1}
      ${DIR_STAGE_1}/bin
      ${DIR_STAGE_1}/lib
      ${DIR_STAGE_1}/libexec
      ${DIR_STAGE_2}
      ${DIR_STAGE_2}/bin
      ${DIR_STAGE_2}/lib
      ${DIR_STAGE_2}/libexec
      ${DIR_STAGE_3}
      ${DIR_STAGE_3}/bin
      ${DIR_STAGE_3}/lib
      ${DIR_STAGE_3}/libexec
      ${DIR_C_XENO}
      ${DIR_C_OBJS}
      ${DIR_C_XENO}/libgcc
      ${DIR_C_OBJS}/libgcc
)



# This function defines the library location and the c-header file location of the requested library
# If the library doesn't exit, it will abort the CMAKE script completely.
# Right now it only supports the structure {dir}/lib {dir}/include.  The MPC, MPFR, and MPFR
# libraries will go away, so no need to support headers that aren't located in standard locations.
# The exception might be iconv, which will remain even with the DLC replacement of GiGi, so we
# might need to support non-standard header locations at that point in time.

function (find_required_library LIBNAME)
   string (TOUPPER ${LIBNAME} SHORTNAME)
   set (REQLIB_NAME "REQLIB_${SHORTNAME}")
   set (HEADER_NAME "HEADER_${SHORTNAME}")
   set (WITH_NAME   "with_${LIBNAME}")

   if ("${${WITH_NAME}}" STREQUAL "")
      find_library (${REQLIB_NAME} ${LIBNAME})
   else ()
      find_library (${REQLIB_NAME} ${LIBNAME} PATHS ${${WITH_NAME}}/lib NO_DEFAULT_PATH)
   endif()

   if ("${REQLIB_NAME}" STREQUAL "${REQLIB_NAME}-NOTFOUND")
      message (FATAL_ERROR "The required ${SHORTNAME} library was not found.  Try the -D WITH_${SHORTNAME}=[dir] option.")
   else ()
      get_filename_component (LFOLDER ${${REQLIB_NAME}} PATH)
      get_filename_component (IFOLDER ${LFOLDER}/../include ABSOLUTE)
      set (${HEADER_NAME} ${IFOLDER} CACHE STRING "Full path of the ${SHORTNAME} library")
   endif ()
endfunction (find_required_library)



# Stage 1 build for compilers where {BUILD} = {HOST} = {TARGET}
# All non-cross compiler builds need to do this step.

if ("${profile}" STREQUAL "standard"       OR
    "${profile}" STREQUAL "standard-base"  OR
    "${profile}" STREQUAL "bootstrap"      OR
    "${profile}" STREQUAL "bootstrap-base" OR
    "${profile}" STREQUAL "")

   find_required_library ("mpc")
   find_required_library ("gmp")
   find_required_library ("mpfr")
   find_required_library ("iconv")

   include (${DIR_CONSTRUCTS}/CMakeLists_libgcc.txt)

endif ()

