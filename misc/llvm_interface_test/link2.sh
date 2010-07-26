#!/bin/tcsh

rm error.log

gnatgcc -c testgen.adb
gnatbind -x testgen.ali

gnatlink testgen.ali \
  /usr/local/lib/libLLVMCore.a \
  /usr/local/lib/libLLVMSupport.a \
  /usr/local/lib/libLLVMSystem.a \
  /usr/lib/libstdc++.a \
  /usr/lib/libm.a \
  /usr/lib/libpthread.a \
  |& tee -a error.log
  
#  /usr/local/lib/libLLVMScalarOpts.a \

  
  
 
