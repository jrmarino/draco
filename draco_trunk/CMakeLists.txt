cmake_minimum_required ( VERSION 2.6 )
project ( DRACO C )


# Set constants
set (DRACO_VERSION "2010-Q3")


# Set defaults
set (PROFILE    "standard")

# Set directory variables
set (DIR_STAGE_1    ${CMAKE_BINARY_DIR}/stage_1)
set (DIR_STAGE_2    ${CMAKE_BINARY_DIR}/stage_2)
set (DIR_STAGE_3    ${CMAKE_BINARY_DIR}/stage_3)
set (DIR_C_OBJS     ${CMAKE_BINARY_DIR}/c_objs)
set (DIR_C_XENO     ${CMAKE_BINARY_DIR}/c_xeno)
set (DIR_CONSTRUCTS ${CMAKE_SOURCE_DIR}/constructs)



# prevent in-source builds for the user's own good
if ("${PROJECT_SOURCE_DIR}" STREQUAL "${PROJECT_BINARY_DIR}")
   message (FATAL_ERROR "Attempted in-source build.  Please run cmake from the build directory.")
endif ()


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
      ${DIR_C_OBJS})


function (find_required_library LIBNAME SHORTNAME)
   set (REQLIB_NAME "REQLIB_${SHORTNAME}")
   set (WITH_NAME   "WITH_${SHORTNAME}")

   if ("${${WITH_NAME}}" STREQUAL "")
      find_library (${REQLIB_NAME} ${LIBNAME})
   else ()
      find_library (${REQLIB_NAME} ${LIBNAME} PATHS ${${WITH_NAME}}/lib NO_DEFAULT_PATH)
   endif()

   if ("${REQLIB_NAME}" STREQUAL "${REQLIB_NAME}-NOTFOUND")
      message (FATAL_ERROR "The required ${SHORTNAME} library was not found.  Try the -D WITH_${SHORTNAME}=[dir] option.")
   endif ()
endfunction (find_required_library)


if ("${PROFILE}" STREQUAL "standard"      OR
    "${PROFILE}" STREQUAL "standard-base" OR
    "${PROFILE}" STREQUAL "bootstrap"     OR
    "${PROFILE}" STREQUAL "bootstrap-base")
#   include (${DIR_CONSTRUCTS}/base_stage1_normal.txt)

   find_required_library ("mpc"   "MPC")
   find_required_library ("gmp"   "GMP")
   find_required_library ("mpfr"  "MPFR")
   find_required_library ("iconv" "ICONV")


endif ()

