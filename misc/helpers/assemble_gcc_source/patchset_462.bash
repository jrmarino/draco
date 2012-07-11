#!/opt/csw/bin/bash

GCCVERSION=4.6.3
DRACO=/export/home/marino/draco/trunk
EXPANSE=/export/home/marino/GCC-TEST
DIFFPROG=/usr/bin/gdiff
GREPPROG=/opt/csw/bin/ggrep
PACHPROG=/usr/bin/gpatch
ADA_SUFFIX=ada
CORE_SUFFIX=core
CXX_SUFFIX=cxx
F95_SUFFIX=fortran
ADA_SUITE_SUFFIX=ada-testsuite
CXX_SUITE_SUFFIX=cxx-testsuite
GCC_SUITE_SUFFIX=gcc-testsuite
FRT_SUITE_SUFFIX=fortran-testsuite
OUTPUT_DIR=${EXPANSE}/patches-${GCCVERSION}
RELEASE_DIR=${EXPANSE}/gcc-${GCCVERSION}
SCRATCH_DIR=${EXPANSE}/scratch

function produce_patch () {
   PATCH_SUFFIX=${1}
   PATCH_FILE=${OUTPUT_DIR}/diff-${PATCH_SUFFIX}
   declare -a DIRECTORY_LIST=("${!2}")

   cd ${DRACO}
   rm -f ${PATCH_FILE}
   for DIR in ${DIRECTORY_LIST[@]}; do
      FILES=${DIR}/*
      for F in ${FILES}; do
         if [ -f ${F} ]; then
            if [ -f ${RELEASE_DIR}/${F} ]; then
               DFMSG=`${DIFFPROG} -q ${RELEASE_DIR}/${F} ${F}`
               if [ -n "${DFMSG}" ] ; then
                  echo "diff ${F}"
                  ${DIFFPROG} -u ${RELEASE_DIR}/${F} ${F} --label=${F}.orig --label=${F} >> ${PATCH_FILE}
               fi
            else
               ${DIFFPROG} -u /dev/null ${F} --label=/dev/null --label=${F} >> ${PATCH_FILE}
            fi
         fi
      done
   done
}

function regenerate_patch () {
   PATCH_SUFFIX=${1}
   FLUX_NAME=${2}
   PATCH_FILE=${OUTPUT_DIR}/diff-${PATCH_SUFFIX}
   FLUX_PATCH=${DRACO}/../misc/gcc_flux_patches/${FLUX_NAME}

   cd ${SCRATCH_DIR}
   IFS=$'\n'
   FILE_LIST=`${GREPPROG} '^+++ ' ${FLUX_PATCH}`
   for FILE in ${FILE_LIST[@]}; do
      FULL_PATH=`echo ${FILE:6} | awk '{print $1}'`
      FILE_PATH=`dirname ${FULL_PATH}`
      FILE_NAME=`basename ${FULL_PATH}`
      mkdir -p ${SCRATCH_DIR}/${FILE_PATH}
      cp ${RELEASE_DIR}/${FILE_PATH}/${FILE_NAME} ${SCRATCH_DIR}/${FILE_PATH}
   done
   ${PACHPROG} -d ${SCRATCH_DIR} -p1 --backup < ${FLUX_PATCH}
   for FILE in ${FILE_LIST[@]}; do
      FULL_PATH=`echo ${FILE:6} | awk '{print $1}'`
      FILE_PATH=`dirname ${FULL_PATH}`
      FILE_NAME=`basename ${FULL_PATH}`
      ${DIFFPROG} -u ${FULL_PATH}.orig ${FULL_PATH} --label=${FULL_PATH}.orig --label=${FULL_PATH} >> ${PATCH_FILE}
   done
}

function remove_file () {
   PATCH_SUFFIX=${1}
   HALF_PATH=${2}
   PATCH_FILE=${OUTPUT_DIR}/diff-${PATCH_SUFFIX}
   FULL_PATH=${RELEASE_DIR}/${HALF_PATH}
   ${DIFFPROG} -u ${FULL_PATH} /dev/null --label=${HALF_PATH} --label=/dev/null >> ${PATCH_FILE}
}

mkdir -p ${OUTPUT_DIR}
pattern="^gcc/ada|^gnattools|^libada"
ada=`cd $DRACO && find * -type d | ${GREPPROG} -E $pattern`
produce_patch ${ADA_SUFFIX} ada[@]

pattern="^gcc/fortran|^libgfortran"
fortran=`cd $DRACO && find * -type d | ${GREPPROG} -E $pattern`
produce_patch ${F95_SUFFIX} fortran[@]
regenerate_patch ${F95_SUFFIX} patch_libgfortran_configure
regenerate_patch ${F95_SUFFIX} patch_libquadmath_Makefile.in

pattern="^gcc/testsuite|^gcc/ada|^gcc/fortran|^gnattools|^libada|^libgfortran|^libstdc..-v3"
core=`cd ${DRACO} && find * -type d | ${GREPPROG} -vE $pattern`
produce_patch ${CORE_SUFFIX} core[@]
regenerate_patch ${CORE_SUFFIX} patch_gcc_configure
regenerate_patch ${CORE_SUFFIX} patch_gcc_Makefile.in
regenerate_patch ${CORE_SUFFIX} patch-libiberty__getpagesize.c
regenerate_patch ${CORE_SUFFIX} patch-libiberty__setprotitle.c
regenerate_patch ${CORE_SUFFIX} patch_configure

pattern="^gcc/testsuite"
suite=`cd $DRACO && find * -type d | ${GREPPROG} -E $pattern`
produce_patch ${ADA_SUITE_SUFFIX} suite[@]
remove_file ${ADA_SUITE_SUFFIX} gcc/testsuite/gnat.dg/unchecked_convert5.adb
remove_file ${ADA_SUITE_SUFFIX} gcc/testsuite/gnat.dg/unchecked_convert6.adb

pattern="^libstdc..-v3"
cplusplus=`cd $DRACO && find * -type d | ${GREPPROG} -E $pattern`
produce_patch ${CXX_SUFFIX} cplusplus[@]

rm -f ${OUTPUT_DIR}/diff-${CXX_SUITE_SUFFIX}
regenerate_patch ${CXX_SUITE_SUFFIX} gxx_ts1.patch
regenerate_patch ${CXX_SUITE_SUFFIX} libstdcxx-testsuite.patch
regenerate_patch ${CXX_SUITE_SUFFIX} libstdc++.exp.patch
regenerate_patch ${CXX_SUITE_SUFFIX} libstdxx_ts_missing_debug_checks.patch
regenerate_patch ${CXX_SUITE_SUFFIX} fix_locales.patch
regenerate_patch ${CXX_SUITE_SUFFIX} fix-ja_JP.eucJP.patch
regenerate_patch ${CXX_SUITE_SUFFIX} fix-hong_kong.patch
regenerate_patch ${CXX_SUITE_SUFFIX} fix-norway.patch
regenerate_patch ${CXX_SUITE_SUFFIX} fix-random-locales.patch

rm -f ${OUTPUT_DIR}/diff-${GCC_SUITE_SUFFIX}
regenerate_patch ${GCC_SUITE_SUFFIX} gcc.pch.exp.patch

rm -f ${OUTPUT_DIR}/diff-${FRT_SUITE_SUFFIX}
regenerate_patch ${FRT_SUITE_SUFFIX} fortran_testsuite.patch

