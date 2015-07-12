#!/usr/local/bin/bash

GCCVERSION=4.9.3
DRACO=/home/marino/github/draco/v4.9
EXPANSE=/home/marino/GCC-TEST
DIFFPROG=/usr/bin/diff
GREPPROG=/usr/bin/grep
PACHPROG=/usr/bin/patch
ADA_SUFFIX=ada
CORE_SUFFIX=core
CXX_SUFFIX=cxx
F95_SUFFIX=fortran
ADA_SUITE_SUFFIX=ada-testsuite
CXX_SUITE_SUFFIX=cxx-testsuite
GCC_SUITE_SUFFIX=gcc-testsuite
FRT_SUITE_SUFFIX=fortran-testsuite
if [ -n "${NETBSD}" ]
then
OUTPUT_DIR=${EXPANSE}/patches-${GCCVERSION}-pkgsrc
else
OUTPUT_DIR=${EXPANSE}/patches-${GCCVERSION}
fi
RELEASE_DIR=${EXPANSE}/gcc-${GCCVERSION}
SCRATCH_DIR=${EXPANSE}/scratch

function reset_patch () {
   PATCH_SUFFIX=${1}
   PATCH_FILE=${OUTPUT_DIR}/diff-${PATCH_SUFFIX}
   cd ${DRACO}
   rm -f ${PATCH_FILE}
}

function produce_patch () {
   PATCH_SUFFIX=${1}
   PATCH_FILE=${OUTPUT_DIR}/diff-${PATCH_SUFFIX}
   declare -a DIRECTORY_LIST=("${!2}")

   cd ${DRACO}
   rm -f ${PATCH_FILE}
   for DIR in ${DIRECTORY_LIST[@]}; do
      echo "     Searching ${DIR}"
      FILES=${DIR}/*
      FILES=`echo ${FILES} | tr " " "\n" | ${GREPPROG} -v openbsd`
      if [ -z "${NETBSD}" ]
      then
         FILES=`echo ${FILES} | tr " " "\n" | ${GREPPROG} -v netbsd`
      fi
      for F in ${FILES}; do
         if [ -f ${F} ]; then
            if [ -f ${RELEASE_DIR}/${F} ]; then
               DFMSG=`${DIFFPROG} -q ${RELEASE_DIR}/${F} ${F}`
               if [ -n "${DFMSG}" ] ; then
                  echo "diff ${F}"
                  ${DIFFPROG} -u ${RELEASE_DIR}/${F} ${F} --label=${F}.orig --label=${F} >> ${PATCH_FILE}
               fi
            else
               echo "==>  ${F}"
               ${DIFFPROG} -u /dev/null ${F} --label=/dev/null --label=${F} \
                 >> ${PATCH_FILE}
            fi
         fi
      done
   done
}

function regenerate_patch () {
   PATCH_SUFFIX=${1}
   FLUX_NAME=${2}
   PATCH_FILE=${OUTPUT_DIR}/diff-${PATCH_SUFFIX}
   FLUX_PATCH=${DRACO}/../misc/flux49/${FLUX_NAME}
   AWKCMD='{if (substr($2,0,2) == "b/") print substr($2,3); else print $2}'
   AWKCM2='NR==1 {if (substr($2,0,2) == "b/") print "-p1"}'

   cd ${SCRATCH_DIR}
   IFS=$'\n'
   FILE_LIST=`${GREPPROG} '^+++ ' ${FLUX_PATCH}`
   for FILE in ${FILE_LIST[@]}; do
      FULL_PATH=`echo ${FILE} | awk "${AWKCMD}"`
      FILE_PATH=`dirname ${FULL_PATH}`
      FILE_NAME=`basename ${FULL_PATH}`
      mkdir -p ${SCRATCH_DIR}/${FILE_PATH}
      cp ${RELEASE_DIR}/${FILE_PATH}/${FILE_NAME} ${SCRATCH_DIR}/${FILE_PATH}
   done
   PATCHLEVEL=`echo ${FILE_LIST} | awk "${AWKCM2}"`
   ${PACHPROG} -d ${SCRATCH_DIR} ${PATCHLEVEL} --backup < ${FLUX_PATCH}
   for FILE in ${FILE_LIST[@]}; do
      FULL_PATH=`echo ${FILE} | awk "${AWKCMD}"`
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

rm -rf ${EXPANSE}/scratch
mkdir -p ${OUTPUT_DIR} ${EXPANSE}/scratch
pattern="^gcc/ada"
ada=`cd $DRACO && find * -type d | sort | ${GREPPROG} -E $pattern`
produce_patch ${ADA_SUFFIX} ada[@]
regenerate_patch ${ADA_SUFFIX} patch-gcc_ada_gcc-interface_Make-lang.in
regenerate_patch ${ADA_SUFFIX} patch-gcc_ada_gcc-interface_Makefile.in
regenerate_patch ${ADA_SUFFIX} patch-gnattools_configure.ac
regenerate_patch ${ADA_SUFFIX} patch-gnattools_configure

pattern="^gcc/fortran"
fortran=`cd $DRACO && find * -type d | sort | ${GREPPROG} -E $pattern`
produce_patch ${F95_SUFFIX} fortran[@]
if [ -n "${NETBSD}" ]
then
regenerate_patch ${F95_SUFFIX} patch-libgfortran_acinclude.m4
regenerate_patch ${F95_SUFFIX} patch-libgfortran_configure
fi

pattern="^gcc/testsuite|^gcc/ada|^gcc/fortran|^libstdc..-v3"
core=`cd ${DRACO} && find * -type d | sort | ${GREPPROG} -vE $pattern`
produce_patch ${CORE_SUFFIX} core[@]
if [ -n "${NETBSD}" ]
then
regenerate_patch ${CORE_SUFFIX} patch-gcc_config.gcc
regenerate_patch ${CORE_SUFFIX} patch-gcc_configure
#regenerate_patch ${CORE_SUFFIX} patch-gcc_builtins.c
fi
regenerate_patch ${CORE_SUFFIX} patch-gcc_Makefile.in
regenerate_patch ${CORE_SUFFIX} patch-libgcc_config.host
regenerate_patch ${CORE_SUFFIX} patch-libcilkrts_runtime_os-unix.c
regenerate_patch ${CORE_SUFFIX} patch-libitm_configure.tgt

pattern="^gcc/testsuite/ada|^gcc/testsuite/gnat.dg"
suite=`cd $DRACO && find * -type d | sort | ${GREPPROG} -E $pattern`
produce_patch ${ADA_SUITE_SUFFIX} suite[@]

pattern="^libstdc..-v3"
cplusplus=`cd $DRACO && find * -type d | sort | ${GREPPROG} -E $pattern`
produce_patch ${CXX_SUFFIX} cplusplus[@]
regenerate_patch ${CXX_SUFFIX} patch-libstdcpp3_acinclude.m4
regenerate_patch ${CXX_SUFFIX} patch-libstdcpp3_configure
regenerate_patch ${CXX_SUFFIX} patch-libstdcpp3_configure.host

pattern="^gcc/testsuite/c-c..-common"
suite=`cd $DRACO && find * -type d | sort | ${GREPPROG} -E $pattern`
produce_patch ${CXX_SUITE_SUFFIX} suite[@]
regenerate_patch ${CXX_SUITE_SUFFIX} libstdc++.exp.patch

pattern="^gcc/testsuite/gcc.dg"
suite=`cd $DRACO && find * -type d | sort | ${GREPPROG} -E $pattern`
produce_patch ${GCC_SUITE_SUFFIX} suite[@]

pattern="^gcc/testsuite/gfortran"
suite=`cd $DRACO && find * -type d | sort | ${GREPPROG} -E $pattern`
produce_patch ${FRT_SUITE_SUFFIX} suite[@]
