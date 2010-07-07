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

cmake_minimum_required ( VERSION 2.8 )
project ( DRACO C )


# Set constants
set (DRACO_VERSION "2010-Q3")


# prevent in-source builds for the user's own good
if ("${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
   message (FATAL_ERROR "Attempted in-source build.  Please run cmake from the build directory.")
endif ()



# set up build directories
set (DIR_CONSTRUCTS ${CMAKE_SOURCE_DIR}/constructs)
include (${DIR_CONSTRUCTS}/cm_directory.txt)



# Retrieve the HOST machine triplet information
include (${DIR_CONSTRUCTS}/cm_triplets.txt)
include (${DIR_CONSTRUCTS}/cm_host_specific.txt)
include (${DIR_CONSTRUCTS}/cm_target_specific.txt)



# Read in GCC version information
file (READ ${DIR_REMNANTS}/gcc/BASE-VER BASEVER)
file (READ ${DIR_REMNANTS}/gcc/DATESTAMP DATESTAMP)
file (READ ${DIR_REMNANTS}/gcc/DEV-PHASE DEVPHASE)



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
   set (P1 "You can not specify the library paths with the cross or xeno profiles.")
   set (P2 "The library must be located at ${with_sysroot}/lib directory.")
   set (P3 "The required ${SHORTNAME} library was not found")
   set (P4 "Try the -D WITH_${SHORTNAME}=[dir] option.")

   if ("${${WITH_NAME}}" STREQUAL "")
      if (${CROSSXENO})
         find_library (${REQLIB_NAME} ${LIBNAME} PATHS ${with_sysroot}/lib NO_DEFAULT_PATH)
      else ()
         find_library (${REQLIB_NAME} ${LIBNAME})
      endif ()
   else ()
      if (${CROSSXENO})
         message (FATAL_ERROR "${P1}\n${P2}")
      else ()
         find_library (${REQLIB_NAME} ${LIBNAME} PATHS ${${WITH_NAME}}/lib NO_DEFAULT_PATH)
      endif ()
   endif()

   if ("${REQLIB_NAME}" STREQUAL "${REQLIB_NAME}-NOTFOUND")
      if (${CROSSXENO})
         message (FATAL_ERROR "${P3}\n${P2}")
      else ()
         message (FATAL_ERROR "${P3}\n${P4}")
      endif ()
   else ()
      get_filename_component (LFOLDER ${${REQLIB_NAME}} PATH)
      get_filename_component (IFOLDER ${LFOLDER}/../include ABSOLUTE)
      set (${HEADER_NAME} ${IFOLDER} CACHE STRING "Full path of the ${SHORTNAME} library")
   endif ()

endfunction (find_required_library)



# Due to how CMAKE works, we will need to load and use these libraries in all scenarios
find_required_library ("mpc")
find_required_library ("gmp")
find_required_library ("mpfr")
find_required_library ("iconv")


# standard warning definitions
set (LOOSE_WARN  -W -Wall -Wwrite-strings -Wcast-qual)
set (C_LOOSE_WARN -Wstrict-prototypes -Wmissing-prototypes)


# generate headers which is a function of the target
include (${DIR_CONSTRUCTS}/cm_gen_headers.txt)



# Stage 1 build for compilers where {BUILD} = {HOST} = {TARGET}
# All non-cross compiler builds need to do this step.

set (CURRENT_STAGE 1)
add_custom_target (compiler ALL)

if (NOT ${CROSSXENO})

   add_custom_target (libgcc_stage1)
   add_custom_target (draco_stage1)
   add_custom_target (frontend_stage1)

   add_subdirectory (${DIR_STAGE_1}/libgcc    ${DIR_STAGE_1}/libgcc)
   add_subdirectory (${DIR_STAGE_1}/libgcc_eh ${DIR_STAGE_1}/libgcc_eh)
   add_subdirectory (${DIR_STAGE_1}/libgcov   ${DIR_STAGE_1}/libgcov)
   add_subdirectory (${DIR_STAGE_1}/crtstuff  ${DIR_STAGE_1}/crtstuff)
   add_subdirectory (${DIR_STAGE_1}/driver    ${DIR_STAGE_1}/driver)
   add_subdirectory (${DIR_STAGE_1}/gnat1bind ${DIR_STAGE_1}/gnat1bind)

   add_dependencies  (compiler libgcc_stage1)
   add_dependencies  (compiler draco_stage1)
   add_dependencies  (compiler frontend_stage1)
endif ()







