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



# set up build directories and define which compilers to use
set (DIR_CONSTRUCTS ${CMAKE_SOURCE_DIR}/constructs)
include (${DIR_CONSTRUCTS}/cm_build_tools.txt)
include (${DIR_CONSTRUCTS}/cm_directory.txt)



# Retrieve the HOST machine triplet information
include (${DIR_CONSTRUCTS}/cm_triplets.txt)
include (${DIR_CONSTRUCTS}/cm_host_specific.txt)
include (${DIR_CONSTRUCTS}/cm_target_specific.txt)



# Read in GCC version information
file (READ ${DIR_REMNANTS}/gcc/BASE-VER BASEVER)
file (READ ${DIR_REMNANTS}/gcc/DATESTAMP DATESTAMP)
file (READ ${DIR_REMNANTS}/gcc/DEV-PHASE DEVPHASE)



############## find_required_library ##############
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
include (${DIR_CONSTRUCTS}/cm_pregeneration.txt)
#include (${DIR_CONSTRUCTS}/cm_gen_headers.txt)


# For standard and bootstrap profiles: {BUILD} = {HOST} = {TARGET}
# For cross and xeno profiles:         {BUILD} = {HOST} BUT {TARGET} differs from {HOST}
# For xeno profile, stage 2:           {BUILD} differs from {HOST}, but {HOST} = {TARGET}
#
# standard  profile: standard system CC or clang, system AS/linker, 1 stage
# bootstrap profile: standard system CC or clang, system AS/linker, 3 stages
# cross     profile: standard system CC or clang, foreign AS/linker, 1 stage
# xeno      profile: stage 1 same as cross stage 1
#                    stage 2 uses stage 1 cross compiler with foreign AS/linker.



# We can't use the standard cmake function "add_library ()" because it uses
# only the compiler specified in {CMAKE_C_COMPILER} which is fixed for the
# entire build.  Instead we will exploit the "add_custom_command ()"
# capability in order gain this function using the compiler specified by the
# global variable {DRACO_C_COMPILER} and the archiver specified by the global
# variable {DRACO_ARCHIVER}.  The {set_files} is a list of full path names.

function (draco_add_library    target_name full_path set_files set_includes set_flags)

   set (drac_libname lib${target_name}.a)
   set (drac_includes "")
   foreach (inclpath as ${set_includes})
      get_filename_component(searchfolder ${inclpath} ABSOLUTE)
      list (APPEND drac_includes "-I${searchfolder}")
   endforeach ()
   set (set_o_files "")
   foreach (full_c_name ${set_files})
      get_filename_component (basename ${full_c_name} NAME_WE)
      add_custom_command (
         OUTPUT  ${basename}.o
         COMMAND ${DRACO_C_COMPILER} ${set_flags} ${drac_includes} -c ${full_c_name}
      )
      list (APPEND set_o_files ${basename}.o)
   endforeach ()

   add_custom_command (
      OUTPUT  ${drac_libname}
      COMMAND ${DRACO_ARCHIVER} ARGS rcs ${drac_libname} ${set_o_files}
      DEPENDS ${set_o_files}
   )

endfunction (draco_add_library)



############## draco_add_executable ##############
# We can't use the standard cmake function "add_executable" for the same
# reason listed for the library above.  This function will do the same
# thing, but use the compiler specified by {DRACO_C_COMPILER}.  if the
# individual object files could be reused, then use instead the
# "draco_build_objects" function with "draco_assemble_executable"

function (draco_add_executable target_name set_files set_includes set_flags)

   set (drac_includes "")
   foreach (inclpath ${set_includes})
      get_filename_component(searchfolder ${inclpath} ABSOLUTE)
      list (APPEND drac_includes "-I${searchfolder}")
   endforeach ()

   add_custom_command (
      OUTPUT  ${target_name}
      COMMAND ${DRACO_C_COMPILER} ARGS ${set_flags} ${drac_includes} -o ${target_name} ${set_files}
   )

endfunction (draco_add_executable)



############## draco_build_objects ##############
# When multiple executables are being built using the same flags and with
# common source files, use this function to build the source files as objects
# and then use "draco_assemble_executable" to bind them together.

function (draco_build_objects    set_files set_includes set_flags)

   set (drac_includes "")
   foreach (inclpath ${set_includes})
      get_filename_component(searchfolder ${inclpath} ABSOLUTE)
      list (APPEND drac_includes "-I${searchfolder}")
   endforeach ()
   foreach (basename ${set_files})
      add_custom_command (
         OUTPUT  ${basename}.o
         COMMAND ${DRACO_C_COMPILER} ${set_flags} ${drac_includes} -c ${basename}.c
      )
   endforeach ()

endfunction (draco_build_objects)



############## draco_assemble_executable ##############
# Pass this function a target name an a list of basenames that correspond to
# existing object files.  This function will create an executable target with
# that information

function (draco_assemble_executable target_name set_files)

   set (set_o_files "")
   foreach (basename ${set_files})
      list (APPEND set_o_files ${basename}.o)
   endforeach ()

   add_custom_target (
      ${target_name}
      ${DRACO_C_COMPILER} -o ${target_name} ${set_o_files}
      DEPENDS ${set_o_files}
   )

endfunction (draco_assemble_executable)





set (CURRENT_STAGE 1)
set (DRACO_C_COMPILER   ${EXTERNAL_C_COMPILER})
set (DRACO_ADA_COMPILER ${EXTERNAL_ADA_COMPILER})
set (ADA_BINDER         ${TOOL_BIND})
set (ADA_LINKER         ${TOOL_LINK})
set (DRACO_ARCHIVER     ${SYSTEM_ARCHIVER})

# These are dependencies common to all profiles.
add_custom_target (compiler ALL)
add_custom_target (insn_generators    )
add_custom_target (library_iberty_s1  )
add_custom_target (insn_products      DEPENDS insn_generators library_iberty_s1)
add_custom_target (driver_s1          )
add_custom_target (library_cpp_s1     )
add_custom_target (library_decnum_s1  )
add_custom_target (library_backend_s1 DEPENDS insn_products)
add_custom_target (backend_s1         DEPENDS library_iberty_s1 library_cpp_s1 library_decnum_s1 library_backend_s1)
add_custom_target (gnat1_s1           DEPENDS backend_s1)
add_custom_target (gnatbind_s1        DEPENDS gnat1_s1)

# cmake scripts for the products common to all profiles
add_subdirectory (${DIR_STAGE_1}/driver       ${DIR_STAGE_1}/driver)
add_subdirectory (${DIR_STAGE_1}/host_headers ${DIR_STAGE_1}/host_headers)
add_subdirectory (${DIR_STAGE_1}/libiberty    ${DIR_STAGE_1}/libiberty)
add_subdirectory (${DIR_STAGE_1}/libcpp       ${DIR_STAGE_1}/libcpp)
add_subdirectory (${DIR_STAGE_1}/libdecnumber ${DIR_STAGE_1}/libdecnumber)
add_subdirectory (${DIR_INSNS}                ${DIR_INSNS})

# Sets up more dependencies for the standard and bootstrap profiles
if (NOT ${CROSSXENO})
   add_custom_target (gnatlink_tmp1      DEPENDS gnatbind_s1)
   add_custom_target (gnatmake_tmp1      DEPENDS gnatbind_s1)

   set (DRACO_ADA_COMPILER               ${DIR_STAGE_1}/sysroot/bin/draco)
   set (ADA_BINDER                       ${DIR_STAGE_1}/sysroot/bin/gnatbind)
   set (ADA_LINKER                       ${DIR_STAGE_1}/sysroot/bin/gnatlink)

   add_custom_target (gnatlink_s1        DEPENDS gnatmake_tmp1)
   add_custom_target (gnatmake_s1        DEPENDS gnatmake_tmp1)
   add_custom_target (gnatlib_s1         DEPENDS gnatlink_s1 gnatmake_s1 driver_s1)
   add_custom_target (gnattools_s1       DEPENDS gnatlib_s1)
endif ()


# in all profiles, the gnattools_s1 will get built
add_dependencies  (compiler gnattools_s1)


# The first set of dependencies for the bootstrap profile are the same as the
# standard profile, so this is just extending the standard profile.
if ("${profile}" STREQUAL "bootstrap")

   set (CURRENT_STAGE 2)

   add_custom_target (driver_s2          DEPENDS gnattools_s1)
   add_custom_target (library_iberty_s2  DEPENDS gnattools_s1)
   add_custom_target (library_cpp_s2     DEPENDS gnattools_s1)
   add_custom_target (library_decnum_s2  DEPENDS gnattools_s1)
   add_custom_target (library_backend_s2 DEPENDS gnattools_s1)
   add_custom_target (backend_s2         DEPENDS library_iberty_s2 library_cpp_s2 library_decnum_s2 library_backend_s2)
   add_custom_target (gnat1_s2           DEPENDS backend_s2)
   add_custom_target (gnatbind_s2        DEPENDS gnat1_s2)

   set (DRACO_ADA_COMPILER               ${DIR_STAGE_2}/sysroot/bin/draco)
   set (ADA_BINDER                       ${DIR_STAGE_2}/sysroot/bin/gnatbind)
   set (ADA_LINKER                       ${DIR_STAGE_2}/sysroot/bin/gnatlink)

   add_custom_target (gnatlink_tmp2      DEPENDS gnatbind_s2)
   add_custom_target (gnatmake_tmp2      DEPENDS gnatbind_s2)
   add_custom_target (gnatlink_s2        DEPENDS gnatmake_tmp2)
   add_custom_target (gnatmake_s2        DEPENDS gnatmake_tmp2)
   add_custom_target (gnatlib_s2         DEPENDS gnatlink_s2 gnatmake_s2 driver_s2)
   add_custom_target (gnattools_s2       DEPENDS gnatlib_s2)

   add_subdirectory (${DIR_STAGE_2}/host_headers ${DIR_STAGE_2}/host_headers)
   add_subdirectory (${DIR_STAGE_2}/libiberty    ${DIR_STAGE_2}/libiberty)

   set (CURRENT_STAGE 3)

   add_custom_target (driver_s3          DEPENDS gnattools_s2)
   add_custom_target (library_iberty_s3  DEPENDS gnattools_s2)
   add_custom_target (library_cpp_s3     DEPENDS gnattools_s2)
   add_custom_target (library_decnum_s3  DEPENDS gnattools_s2)
   add_custom_target (library_backend_s3 DEPENDS gnattools_s2)
   add_custom_target (backend_s3         DEPENDS library_iberty_s3 library_cpp_s3 library_decnum_s3 library_backend_s3)
   add_custom_target (gnat1_s3           DEPENDS backend_s3)
   add_custom_target (gnatbind_s3        DEPENDS gnat1_s3)

   set (DRACO_ADA_COMPILER               ${DIR_STAGE_3}/sysroot/bin/draco)
   set (ADA_BINDER                       ${DIR_STAGE_3}/sysroot/bin/gnatbind)
   set (ADA_LINKER                       ${DIR_STAGE_3}/sysroot/bin/gnatlink)

   add_custom_target (gnatlink_tmp3      DEPENDS gnatbind_s3)
   add_custom_target (gnatmake_tmp3      DEPENDS gnatbind_s3)
   add_custom_target (gnatlink_s3        DEPENDS gnatmake_tmp3)
   add_custom_target (gnatmake_s3        DEPENDS gnatmake_tmp3)
   add_custom_target (gnatlib_s3         DEPENDS gnatlink_s3 gnatmake_s3 driver_s3)
   add_custom_target (gnattools_s3       DEPENDS gnatlib_s3)

   add_dependencies  (compiler gnattools_s3)
   add_subdirectory (${DIR_STAGE_3}/host_headers ${DIR_STAGE_3}/host_headers)
   add_subdirectory (${DIR_STAGE_3}/libiberty    ${DIR_STAGE_3}/libiberty)

endif ()

# The cross profile has a different build profile because the system compiler
# has to be used for the entire build
if (${CROSSXENO})

   add_custom_target (gnatlink_s1        DEPENDS gnatbind_s1)
   add_custom_target (gnatmake_s1        DEPENDS gnatbind_s1)
   add_custom_target (gnatlib_s1         DEPENDS gnatlink_s1 gnatmake_s1 driver_s1)
   add_custom_target (gnattools_s1       DEPENDS gnatlib_s1)

endif ()


# The second stage of the xeno profile is identical to the first stage of the
# cross/xeno profile.  Both use the same compilers for the entire stage.
if ("${profile}" STREQUAL "xeno")

   set (CURRENT_STAGE 2)
   set (DRACO_C_COMPILER        ${CROSS_C_COMPILER})
   set (DRACO_ADA_COMPILER      ${DIR_STAGE_1}/sysroot/bin/draco)
   set (ADA_BINDER              ${DIR_STAGE_1}/sysroot/bin/gnatbind)
   set (ADA_LINKER              ${DIR_STAGE_1}/sysroot/bin/gnatlink)
   set (DRACO_ARCHIVER          ${with_sysroot}/bin/ar)

   add_custom_target (driver_s2          DEPENDS gnattools_s1)
   add_custom_target (library_iberty_s2  DEPENDS gnattools_s1)
   add_custom_target (library_cpp_s2     DEPENDS gnattools_s1)
   add_custom_target (library_decnum_s2  DEPENDS gnattools_s1)
   add_custom_target (library_backend_s2 DEPENDS gnattools_s1)
   add_custom_target (backend_s2         DEPENDS library_iberty_s2 library_cpp_s2 library_decnum_s2 library_backend_s2)
   add_custom_target (gnat1_s2           DEPENDS backend_s2)
   add_custom_target (gnatbind_s2        DEPENDS gnat1_s2)
   add_custom_target (gnatlink_s2        DEPENDS gnatbind_s2)
   add_custom_target (gnatmake_s2        DEPENDS gnatbind_s2)
   add_custom_target (gnatlib_s2         DEPENDS gnatlink_s2 gnatmake_s2 driver_s2)
   add_custom_target (gnattools_s2       DEPENDS gnatlib_s2)

   add_dependencies  (compiler gnattools_s2)
   add_subdirectory (${DIR_STAGE_2}/host_headers ${DIR_STAGE_2}/host_headers)
   add_subdirectory (${DIR_STAGE_2}/libiberty    ${DIR_STAGE_2}/libiberty)

endif ()























#if (NOT ${CROSSXENO})
#
#   add_custom_target (libgcc_stage1)
#   add_custom_target (draco_stage1)
#   add_custom_target (frontend_stage1)
#
#   add_subdirectory (${DIR_STAGE_1}/libiberty ${DIR_STAGE_1}/libiberty)
#   add_subdirectory (${DIR_STAGE_1}/libgcc    ${DIR_STAGE_1}/libgcc)
#   add_subdirectory (${DIR_STAGE_1}/libgcc_eh ${DIR_STAGE_1}/libgcc_eh)
#   add_subdirectory (${DIR_STAGE_1}/libgcov   ${DIR_STAGE_1}/libgcov)
#   add_subdirectory (${DIR_STAGE_1}/crtstuff  ${DIR_STAGE_1}/crtstuff)
#   add_subdirectory (${DIR_STAGE_1}/driver    ${DIR_STAGE_1}/driver)
#   add_subdirectory (${DIR_STAGE_1}/insns     ${DIR_STAGE_1}/insns)
#   add_subdirectory (${DIR_STAGE_1}/gnat1bind ${DIR_STAGE_1}/gnat1bind)
#
#   add_dependencies  (compiler libgcc_stage1)
#   add_dependencies  (compiler draco_stage1)
#   add_dependencies  (compiler frontend_stage1)
#endif ()



