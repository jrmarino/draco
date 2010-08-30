--
--  DRACO ADA COMPILER
--  LLVM Library C Interface
--
--
--  Copyright (c) 2010, John Marino (www.auroraux.org)
--  All rights reserved.
--
--  Permission to use, copy, modify, and/or distribute this software for any
--  purpose with or without fee is hereby granted, provided that the above
--  copyright notice and this permission notice appear in all copies.
--
--  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
--  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
--  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
--  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
--  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
--  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
--  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.


with Interfaces.C; use Interfaces.C;
with System;
with Interfaces.C.Strings;
with Interfaces.C.Extensions;

package Core_h is

  --/*===-- llvm-c/Core.h - Core Library C Interface ------------------*- C -*-===*\
  --|*                     The LLVM Compiler Infrastructure                       *|
  --|*                                                                            *|
  --|* This file is distributed under the University of Illinois Open Source      *|
  --|* License. See LICENSE.TXT for details.                                      *|
  --|*                                                                            *|
  --|*===----------------------------------------------------------------------===*|
  --|*                                                                            *|
  --|* This header declares the C interface to libLLVMCore.a, which implements    *|
  --|* the LLVM intermediate representation.                                      *|
  --|*                                                                            *|
  --|* LLVM uses a polymorphic type hierarchy which C cannot represent, therefore *|
  --|* parameters must be passed as base types. Despite the declared types, most  *|
  --|* of the functions provided operate only on branches of the type hierarchy.  *|
  --|* The declared parameter names are descriptive and specify which type is     *|
  --|* required. Additionally, each type hierarchy is documented along with the   *|
  --|* functions that operate upon it. For more detail, refer to LLVM's C++ code. *|
  --|* If in doubt, refer to Core.cpp, which performs paramter downcasts in the   *|
  --|* form unwrap<RequiredType>(Param).                                          *|
  --|*                                                                            *|
  --|* Many exotic languages can interoperate with C code but have a harder time  *|
  --|* with C++ due to name mangling. So in addition to C, this interface enables *|
  --|* tools written in such languages.                                           *|
  --|*                                                                            *|
  --|* When included into a C++ source file, also declares 'wrap' and 'unwrap'    *|
  --|* helpers to perform opaque reference<-->pointer conversions. These helpers  *|
  --|* are shorter and more tightly typed than writing the casts by hand when     *|
  --|* authoring bindings. In assert builds, they will do runtime type checking.  *|
  --|*                                                                            *|
  --\*===----------------------------------------------------------------------===

  -- The only type referenced in the System/DataTypes.h header is uint8_t, which
  --   is fixed regardless of system, so we'll roll our own to to make this
  --   interface target independent.

   subtype uint8_t is unsigned_char;  -- Core.h:39

  -- Need these includes to support the LLVM 'cast' template for the C++ 'wrap'
  --   and 'unwrap' conversion functions.

   subtype LLVMBool is int;  -- Core.h:52

  -- Opaque types.
  --*
  -- * The top-level container for all LLVM global data.  See the LLVMContext class.
  --

   --  skipped empty struct LLVMOpaqueContext

   type LLVMContextRef is new System.Address;  -- Core.h:59

  --*
  -- * The top-level container for all other LLVM Intermediate Representation (IR)
  -- * objects. See the llvm::Module class.
  --

   --  skipped empty struct LLVMOpaqueModule

   type LLVMModuleRef is new System.Address;  -- Core.h:65

  --*
  -- * Each value in the LLVM IR has a type, an LLVMTypeRef. See the llvm::Type
  -- * class.
  --

   --  skipped empty struct LLVMOpaqueType

   type LLVMTypeRef is new System.Address;  -- Core.h:71

  --*
  -- * When building recursive types using LLVMRefineType, LLVMTypeRef values may
  -- * become invalid; use LLVMTypeHandleRef to resolve this problem. See the
  -- * llvm::AbstractTypeHolder class.
  --

   --  skipped empty struct LLVMOpaqueTypeHandle

   type LLVMTypeHandleRef is new System.Address;  -- Core.h:78

   --  skipped empty struct LLVMOpaqueValue

   type LLVMValueRef is new System.Address;  -- Core.h:80

   --  skipped empty struct LLVMOpaqueBasicBlock

   type LLVMBasicBlockRef is new System.Address;  -- Core.h:81

   --  skipped empty struct LLVMOpaqueBuilder

   type LLVMBuilderRef is new System.Address;  -- Core.h:82

  -- Interface used to provide a module to JIT or interpreter.  This is now just a
  -- * synonym for llvm::Module, but we have to keep using the different type to
  -- * keep binary compatibility.
  --

   --  skipped empty struct LLVMOpaqueModuleProvider

   type LLVMModuleProviderRef is new System.Address;  -- Core.h:88

  -- Used to provide a module to JIT or interpreter.
  -- * See the llvm::MemoryBuffer class.
  --

   --  skipped empty struct LLVMOpaqueMemoryBuffer

   type LLVMMemoryBufferRef is new System.Address;  -- Core.h:93

  --* See the llvm::PassManagerBase class.
   --  skipped empty struct LLVMOpaquePassManager

   type LLVMPassManagerRef is new System.Address;  -- Core.h:96

  --* Used to get the users and usees of a Value. See the llvm::Use class.
   --  skipped empty struct LLVMOpaqueUse

   type LLVMUseRef is new System.Address;  -- Core.h:99

   subtype LLVMAttribute is unsigned;
   LLVMZExtAttribute            : constant LLVMAttribute := 1;
   LLVMSExtAttribute            : constant LLVMAttribute := 2;
   LLVMNoReturnAttribute        : constant LLVMAttribute := 4;
   LLVMInRegAttribute           : constant LLVMAttribute := 8;
   LLVMStructRetAttribute       : constant LLVMAttribute := 16;
   LLVMNoUnwindAttribute        : constant LLVMAttribute := 32;
   LLVMNoAliasAttribute         : constant LLVMAttribute := 64;
   LLVMByValAttribute           : constant LLVMAttribute := 128;
   LLVMNestAttribute            : constant LLVMAttribute := 256;
   LLVMReadNoneAttribute        : constant LLVMAttribute := 512;
   LLVMReadOnlyAttribute        : constant LLVMAttribute := 1024;
   LLVMNoInlineAttribute        : constant LLVMAttribute := 2048;
   LLVMAlwaysInlineAttribute    : constant LLVMAttribute := 4096;
   LLVMOptimizeForSizeAttribute : constant LLVMAttribute := 8192;
   LLVMStackProtectAttribute    : constant LLVMAttribute := 16384;
   LLVMStackProtectReqAttribute : constant LLVMAttribute := 32768;
   LLVMAlignment                : constant LLVMAttribute := 2031616;
   LLVMNoCaptureAttribute       : constant LLVMAttribute := 2097152;
   LLVMNoRedZoneAttribute       : constant LLVMAttribute := 4194304;
   LLVMNoImplicitFloatAttribute : constant LLVMAttribute := 8388608;
   LLVMNakedAttribute           : constant LLVMAttribute := 16777216;
   LLVMInlineHintAttribute      : constant LLVMAttribute := 33554432;
   LLVMStackAlignment           : constant LLVMAttribute := 469762048;  -- Core.h:125

  -- Terminator Instructions
  -- Standard Binary Operators
  -- Logical Operators
  -- Memory Operators
  -- Cast Operators
  -- Other Operators
  -- UserOp1
  -- UserOp2
   subtype LLVMOpcode is unsigned;
   LLVMRet            : constant LLVMOpcode := 1;
   LLVMBr             : constant LLVMOpcode := 2;
   LLVMSwitch         : constant LLVMOpcode := 3;
   LLVMIndirectBr     : constant LLVMOpcode := 4;
   LLVMInvoke         : constant LLVMOpcode := 5;
   LLVMUnwind         : constant LLVMOpcode := 6;
   LLVMUnreachable    : constant LLVMOpcode := 7;
   LLVMAdd            : constant LLVMOpcode := 8;
   LLVMFAdd           : constant LLVMOpcode := 9;
   LLVMSub            : constant LLVMOpcode := 10;
   LLVMFSub           : constant LLVMOpcode := 11;
   LLVMMul            : constant LLVMOpcode := 12;
   LLVMFMul           : constant LLVMOpcode := 13;
   LLVMUDiv           : constant LLVMOpcode := 14;
   LLVMSDiv           : constant LLVMOpcode := 15;
   LLVMFDiv           : constant LLVMOpcode := 16;
   LLVMURem           : constant LLVMOpcode := 17;
   LLVMSRem           : constant LLVMOpcode := 18;
   LLVMFRem           : constant LLVMOpcode := 19;
   LLVMShl            : constant LLVMOpcode := 20;
   LLVMLShr           : constant LLVMOpcode := 21;
   LLVMAShr           : constant LLVMOpcode := 22;
   LLVMAnd            : constant LLVMOpcode := 23;
   LLVMOr             : constant LLVMOpcode := 24;
   LLVMXor            : constant LLVMOpcode := 25;
   LLVMAlloca         : constant LLVMOpcode := 26;
   LLVMLoad           : constant LLVMOpcode := 27;
   LLVMStore          : constant LLVMOpcode := 28;
   LLVMGetElementPtr  : constant LLVMOpcode := 29;
   LLVMTrunc          : constant LLVMOpcode := 30;
   LLVMZExt           : constant LLVMOpcode := 31;
   LLVMSExt           : constant LLVMOpcode := 32;
   LLVMFPToUI         : constant LLVMOpcode := 33;
   LLVMFPToSI         : constant LLVMOpcode := 34;
   LLVMUIToFP         : constant LLVMOpcode := 35;
   LLVMSIToFP         : constant LLVMOpcode := 36;
   LLVMFPTrunc        : constant LLVMOpcode := 37;
   LLVMFPExt          : constant LLVMOpcode := 38;
   LLVMPtrToInt       : constant LLVMOpcode := 39;
   LLVMIntToPtr       : constant LLVMOpcode := 40;
   LLVMBitCast        : constant LLVMOpcode := 41;
   LLVMICmp           : constant LLVMOpcode := 42;
   LLVMFCmp           : constant LLVMOpcode := 43;
   LLVMPHI            : constant LLVMOpcode := 44;
   LLVMCall           : constant LLVMOpcode := 45;
   LLVMSelect         : constant LLVMOpcode := 46;
   LLVMVAArg          : constant LLVMOpcode := 49;
   LLVMExtractElement : constant LLVMOpcode := 50;
   LLVMInsertElement  : constant LLVMOpcode := 51;
   LLVMShuffleVector  : constant LLVMOpcode := 52;
   LLVMExtractValue   : constant LLVMOpcode := 53;
   LLVMInsertValue    : constant LLVMOpcode := 54;  -- Core.h:193

  --*< type with no size
  --*< 32 bit floating point type
  --*< 64 bit floating point type
  --*< 80 bit floating point type (X87)
  --*< 128 bit floating point type (112-bit mantissa)
  --*< 128 bit floating point type (two 64-bits)
  --*< Labels
  --*< Arbitrary bit width integers
  --*< Functions
  --*< Structures
  --*< Arrays
  --*< Pointers
  --*< Opaque: type with unknown structure
  --*< SIMD 'packed' format, or other vector type
  --*< Metadata
  --*< Unions
   type LLVMTypeKind is
     (LLVMVoidTypeKind,
      LLVMFloatTypeKind,
      LLVMDoubleTypeKind,
      LLVMX86_FP80TypeKind,
      LLVMFP128TypeKind,
      LLVMPPC_FP128TypeKind,
      LLVMLabelTypeKind,
      LLVMIntegerTypeKind,
      LLVMFunctionTypeKind,
      LLVMStructTypeKind,
      LLVMArrayTypeKind,
      LLVMPointerTypeKind,
      LLVMOpaqueTypeKind,
      LLVMVectorTypeKind,
      LLVMMetadataTypeKind,
      LLVMUnionTypeKind);
   pragma Convention (C, LLVMTypeKind);  -- Core.h:212

  --*< Externally visible function
  --*< Keep one copy of function when linking (inline)
  --*< Same, but only replaced by something
  --                            equivalent.

  --*< Keep one copy of function when linking (weak)
  --*< Same, but only replaced by something
  --                            equivalent.

  --*< Special purpose, only applies to global arrays
  --*< Rename collisions when linking (static
  --                               functions)

  --*< Like Internal, but omit from symbol table
  --*< Function to be imported from DLL
  --*< Function to be accessible from DLL
  --*< ExternalWeak linkage description
  --*< Obsolete
  --*< Tentative definitions
  --*< Like Private, but linker removes.
   type LLVMLinkage is
     (LLVMExternalLinkage,
      LLVMAvailableExternallyLinkage,
      LLVMLinkOnceAnyLinkage,
      LLVMLinkOnceODRLinkage,
      LLVMWeakAnyLinkage,
      LLVMWeakODRLinkage,
      LLVMAppendingLinkage,
      LLVMInternalLinkage,
      LLVMPrivateLinkage,
      LLVMDLLImportLinkage,
      LLVMDLLExportLinkage,
      LLVMExternalWeakLinkage,
      LLVMGhostLinkage,
      LLVMCommonLinkage,
      LLVMLinkerPrivateLinkage);
   pragma Convention (C, LLVMLinkage);  -- Core.h:233

  --*< The GV is visible
  --*< The GV is hidden
  --*< The GV is protected
   type LLVMVisibility is
     (LLVMDefaultVisibility,
      LLVMHiddenVisibility,
      LLVMProtectedVisibility);
   pragma Convention (C, LLVMVisibility);  -- Core.h:239

   subtype LLVMCallConv is unsigned;
   LLVMCCallConv           : constant LLVMCallConv := 0;
   LLVMFastCallConv        : constant LLVMCallConv := 8;
   LLVMColdCallConv        : constant LLVMCallConv := 9;
   LLVMX86StdcallCallConv  : constant LLVMCallConv := 64;
   LLVMX86FastcallCallConv : constant LLVMCallConv := 65;  -- Core.h:247

  --*< equal
  --*< not equal
  --*< unsigned greater than
  --*< unsigned greater or equal
  --*< unsigned less than
  --*< unsigned less or equal
  --*< signed greater than
  --*< signed greater or equal
  --*< signed less than
  --*< signed less or equal
   subtype LLVMIntPredicate is unsigned;
   LLVMIntEQ  : constant LLVMIntPredicate := 32;
   LLVMIntNE  : constant LLVMIntPredicate := 33;
   LLVMIntUGT : constant LLVMIntPredicate := 34;
   LLVMIntUGE : constant LLVMIntPredicate := 35;
   LLVMIntULT : constant LLVMIntPredicate := 36;
   LLVMIntULE : constant LLVMIntPredicate := 37;
   LLVMIntSGT : constant LLVMIntPredicate := 38;
   LLVMIntSGE : constant LLVMIntPredicate := 39;
   LLVMIntSLT : constant LLVMIntPredicate := 40;
   LLVMIntSLE : constant LLVMIntPredicate := 41;  -- Core.h:260

  --*< Always false (always folded)
  --*< True if ordered and equal
  --*< True if ordered and greater than
  --*< True if ordered and greater than or equal
  --*< True if ordered and less than
  --*< True if ordered and less than or equal
  --*< True if ordered and operands are unequal
  --*< True if ordered (no nans)
  --*< True if unordered: isnan(X) | isnan(Y)
  --*< True if unordered or equal
  --*< True if unordered or greater than
  --*< True if unordered, greater than, or equal
  --*< True if unordered or less than
  --*< True if unordered, less than, or equal
  --*< True if unordered or not equal
  --*< Always true (always folded)
   type LLVMRealPredicate is
     (LLVMRealPredicateFalse,
      LLVMRealOEQ,
      LLVMRealOGT,
      LLVMRealOGE,
      LLVMRealOLT,
      LLVMRealOLE,
      LLVMRealONE,
      LLVMRealORD,
      LLVMRealUNO,
      LLVMRealUEQ,
      LLVMRealUGT,
      LLVMRealUGE,
      LLVMRealULT,
      LLVMRealULE,
      LLVMRealUNE,
      LLVMRealPredicateTrue);
   pragma Convention (C, LLVMRealPredicate);  -- Core.h:279

  --===-- Error handling ----------------------------------------------------===
   procedure LLVMDisposeMessage (arg1 : Interfaces.C.Strings.chars_ptr);  -- Core.h:284
   pragma Import (C, LLVMDisposeMessage, "LLVMDisposeMessage");

  --===-- Contexts ----------------------------------------------------------===
  -- Create and destroy contexts.
   function LLVMContextCreate return LLVMContextRef;  -- Core.h:290
   pragma Import (C, LLVMContextCreate, "LLVMContextCreate");

   function LLVMGetGlobalContext return LLVMContextRef;  -- Core.h:291
   pragma Import (C, LLVMGetGlobalContext, "LLVMGetGlobalContext");

   procedure LLVMContextDispose (arg1 : LLVMContextRef);  -- Core.h:292
   pragma Import (C, LLVMContextDispose, "LLVMContextDispose");

   function LLVMGetMDKindIDInContext
     (arg1 : LLVMContextRef;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned) return unsigned;  -- Core.h:294
   pragma Import (C, LLVMGetMDKindIDInContext, "LLVMGetMDKindIDInContext");

   function LLVMGetMDKindID (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : unsigned) return unsigned;  -- Core.h:296
   pragma Import (C, LLVMGetMDKindID, "LLVMGetMDKindID");

  --===-- Modules -----------------------------------------------------------===
  -- Create and destroy modules.
  --* See llvm::Module::Module.
   function LLVMModuleCreateWithName (arg1 : Interfaces.C.Strings.chars_ptr) return LLVMModuleRef;  -- Core.h:302
   pragma Import (C, LLVMModuleCreateWithName, "LLVMModuleCreateWithName");

   function LLVMModuleCreateWithNameInContext (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : LLVMContextRef) return LLVMModuleRef;  -- Core.h:303
   pragma Import (C, LLVMModuleCreateWithNameInContext, "LLVMModuleCreateWithNameInContext");

  --* See llvm::Module::~Module.
   procedure LLVMDisposeModule (arg1 : LLVMModuleRef);  -- Core.h:307
   pragma Import (C, LLVMDisposeModule, "LLVMDisposeModule");

  --* Data layout. See Module::getDataLayout.
   function LLVMGetDataLayout (arg1 : LLVMModuleRef) return Interfaces.C.Strings.chars_ptr;  -- Core.h:310
   pragma Import (C, LLVMGetDataLayout, "LLVMGetDataLayout");

   procedure LLVMSetDataLayout (arg1 : LLVMModuleRef; arg2 : Interfaces.C.Strings.chars_ptr);  -- Core.h:311
   pragma Import (C, LLVMSetDataLayout, "LLVMSetDataLayout");

  --* Target triple. See Module::getTargetTriple.
   function LLVMGetTarget (arg1 : LLVMModuleRef) return Interfaces.C.Strings.chars_ptr;  -- Core.h:314
   pragma Import (C, LLVMGetTarget, "LLVMGetTarget");

   procedure LLVMSetTarget (arg1 : LLVMModuleRef; arg2 : Interfaces.C.Strings.chars_ptr);  -- Core.h:315
   pragma Import (C, LLVMSetTarget, "LLVMSetTarget");

  --* See Module::addTypeName.
   function LLVMAddTypeName
     (arg1 : LLVMModuleRef;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : LLVMTypeRef) return LLVMBool;  -- Core.h:318
   pragma Import (C, LLVMAddTypeName, "LLVMAddTypeName");

   procedure LLVMDeleteTypeName (arg1 : LLVMModuleRef; arg2 : Interfaces.C.Strings.chars_ptr);  -- Core.h:319
   pragma Import (C, LLVMDeleteTypeName, "LLVMDeleteTypeName");

   function LLVMGetTypeByName (arg1 : LLVMModuleRef; arg2 : Interfaces.C.Strings.chars_ptr) return LLVMTypeRef;  -- Core.h:320
   pragma Import (C, LLVMGetTypeByName, "LLVMGetTypeByName");

  --* See Module::dump.
   procedure LLVMDumpModule (arg1 : LLVMModuleRef);  -- Core.h:323
   pragma Import (C, LLVMDumpModule, "LLVMDumpModule");

  --===-- Types -------------------------------------------------------------===
  -- LLVM types conform to the following hierarchy:
  -- *
  -- *   types:
  -- *     integer type
  -- *     real type
  -- *     function type
  -- *     sequence types:
  -- *       array type
  -- *       pointer type
  -- *       vector type
  -- *     void type
  -- *     label type
  -- *     opaque type
  --

  --* See llvm::LLVMTypeKind::getTypeID.
   function LLVMGetTypeKind (arg1 : LLVMTypeRef) return LLVMTypeKind;  -- Core.h:344
   pragma Import (C, LLVMGetTypeKind, "LLVMGetTypeKind");

  --* See llvm::LLVMType::getContext.
   function LLVMGetTypeContext (arg1 : LLVMTypeRef) return LLVMContextRef;  -- Core.h:347
   pragma Import (C, LLVMGetTypeContext, "LLVMGetTypeContext");

  -- Operations on integer types
   function LLVMInt1TypeInContext (arg1 : LLVMContextRef) return LLVMTypeRef;  -- Core.h:350
   pragma Import (C, LLVMInt1TypeInContext, "LLVMInt1TypeInContext");

   function LLVMInt8TypeInContext (arg1 : LLVMContextRef) return LLVMTypeRef;  -- Core.h:351
   pragma Import (C, LLVMInt8TypeInContext, "LLVMInt8TypeInContext");

   function LLVMInt16TypeInContext (arg1 : LLVMContextRef) return LLVMTypeRef;  -- Core.h:352
   pragma Import (C, LLVMInt16TypeInContext, "LLVMInt16TypeInContext");

   function LLVMInt32TypeInContext (arg1 : LLVMContextRef) return LLVMTypeRef;  -- Core.h:353
   pragma Import (C, LLVMInt32TypeInContext, "LLVMInt32TypeInContext");

   function LLVMInt64TypeInContext (arg1 : LLVMContextRef) return LLVMTypeRef;  -- Core.h:354
   pragma Import (C, LLVMInt64TypeInContext, "LLVMInt64TypeInContext");

   function LLVMIntTypeInContext (arg1 : LLVMContextRef; arg2 : unsigned) return LLVMTypeRef;  -- Core.h:355
   pragma Import (C, LLVMIntTypeInContext, "LLVMIntTypeInContext");

   function LLVMInt1Type return LLVMTypeRef;  -- Core.h:357
   pragma Import (C, LLVMInt1Type, "LLVMInt1Type");

   function LLVMInt8Type return LLVMTypeRef;  -- Core.h:358
   pragma Import (C, LLVMInt8Type, "LLVMInt8Type");

   function LLVMInt16Type return LLVMTypeRef;  -- Core.h:359
   pragma Import (C, LLVMInt16Type, "LLVMInt16Type");

   function LLVMInt32Type return LLVMTypeRef;  -- Core.h:360
   pragma Import (C, LLVMInt32Type, "LLVMInt32Type");

   function LLVMInt64Type return LLVMTypeRef;  -- Core.h:361
   pragma Import (C, LLVMInt64Type, "LLVMInt64Type");

   function LLVMIntType (arg1 : unsigned) return LLVMTypeRef;  -- Core.h:362
   pragma Import (C, LLVMIntType, "LLVMIntType");

   function LLVMGetIntTypeWidth (arg1 : LLVMTypeRef) return unsigned;  -- Core.h:363
   pragma Import (C, LLVMGetIntTypeWidth, "LLVMGetIntTypeWidth");

  -- Operations on real types
   function LLVMFloatTypeInContext (arg1 : LLVMContextRef) return LLVMTypeRef;  -- Core.h:366
   pragma Import (C, LLVMFloatTypeInContext, "LLVMFloatTypeInContext");

   function LLVMDoubleTypeInContext (arg1 : LLVMContextRef) return LLVMTypeRef;  -- Core.h:367
   pragma Import (C, LLVMDoubleTypeInContext, "LLVMDoubleTypeInContext");

   function LLVMX86FP80TypeInContext (arg1 : LLVMContextRef) return LLVMTypeRef;  -- Core.h:368
   pragma Import (C, LLVMX86FP80TypeInContext, "LLVMX86FP80TypeInContext");

   function LLVMFP128TypeInContext (arg1 : LLVMContextRef) return LLVMTypeRef;  -- Core.h:369
   pragma Import (C, LLVMFP128TypeInContext, "LLVMFP128TypeInContext");

   function LLVMPPCFP128TypeInContext (arg1 : LLVMContextRef) return LLVMTypeRef;  -- Core.h:370
   pragma Import (C, LLVMPPCFP128TypeInContext, "LLVMPPCFP128TypeInContext");

   function LLVMFloatType return LLVMTypeRef;  -- Core.h:372
   pragma Import (C, LLVMFloatType, "LLVMFloatType");

   function LLVMDoubleType return LLVMTypeRef;  -- Core.h:373
   pragma Import (C, LLVMDoubleType, "LLVMDoubleType");

   function LLVMX86FP80Type return LLVMTypeRef;  -- Core.h:374
   pragma Import (C, LLVMX86FP80Type, "LLVMX86FP80Type");

   function LLVMFP128Type return LLVMTypeRef;  -- Core.h:375
   pragma Import (C, LLVMFP128Type, "LLVMFP128Type");

   function LLVMPPCFP128Type return LLVMTypeRef;  -- Core.h:376
   pragma Import (C, LLVMPPCFP128Type, "LLVMPPCFP128Type");

  -- Operations on function types
   function LLVMFunctionType
     (ReturnType : LLVMTypeRef;
      ParamTypes : System.Address;
      ParamCount : unsigned;
      IsVarArg   : LLVMBool) return LLVMTypeRef;  -- Core.h:379
   pragma Import (C, LLVMFunctionType, "LLVMFunctionType");

   function LLVMIsFunctionVarArg (arg1 : LLVMTypeRef) return LLVMBool;  -- Core.h:382
   pragma Import (C, LLVMIsFunctionVarArg, "LLVMIsFunctionVarArg");

   function LLVMGetReturnType (arg1 : LLVMTypeRef) return LLVMTypeRef;  -- Core.h:383
   pragma Import (C, LLVMGetReturnType, "LLVMGetReturnType");

   function LLVMCountParamTypes (arg1 : LLVMTypeRef) return unsigned;  -- Core.h:384
   pragma Import (C, LLVMCountParamTypes, "LLVMCountParamTypes");

   procedure LLVMGetParamTypes (arg1 : LLVMTypeRef; arg2 : System.Address);  -- Core.h:385
   pragma Import (C, LLVMGetParamTypes, "LLVMGetParamTypes");

  -- Operations on struct types
   function LLVMStructTypeInContext
     (C            : LLVMContextRef;
      ElementTypes : System.Address;
      ElementCount : unsigned;
      Packed       : LLVMBool) return LLVMTypeRef;  -- Core.h:388
   pragma Import (C, LLVMStructTypeInContext, "LLVMStructTypeInContext");

   function LLVMStructType
     (ElementTypes : System.Address;
      ElementCount : unsigned;
      Packed       : LLVMBool) return LLVMTypeRef;  -- Core.h:390
   pragma Import (C, LLVMStructType, "LLVMStructType");

   function LLVMCountStructElementTypes (StructTy : LLVMTypeRef) return unsigned;  -- Core.h:392
   pragma Import (C, LLVMCountStructElementTypes, "LLVMCountStructElementTypes");

   procedure LLVMGetStructElementTypes (StructTy : LLVMTypeRef; Dest : System.Address);  -- Core.h:393
   pragma Import (C, LLVMGetStructElementTypes, "LLVMGetStructElementTypes");

   function LLVMIsPackedStruct (StructTy : LLVMTypeRef) return LLVMBool;  -- Core.h:394
   pragma Import (C, LLVMIsPackedStruct, "LLVMIsPackedStruct");

  -- Operations on union types
   function LLVMUnionTypeInContext
     (C            : LLVMContextRef;
      ElementTypes : System.Address;
      ElementCount : unsigned) return LLVMTypeRef;  -- Core.h:397
   pragma Import (C, LLVMUnionTypeInContext, "LLVMUnionTypeInContext");

   function LLVMUnionType (ElementTypes : System.Address; 
                           ElementCount : unsigned) return LLVMTypeRef;  -- Core.h:399
   pragma Import (C, LLVMUnionType, "LLVMUnionType");

   function LLVMCountUnionElementTypes (UnionTy : LLVMTypeRef) return unsigned;  -- Core.h:400
   pragma Import (C, LLVMCountUnionElementTypes, "LLVMCountUnionElementTypes");

   procedure LLVMGetUnionElementTypes (UnionTy : LLVMTypeRef; Dest : System.Address);  -- Core.h:401
   pragma Import (C, LLVMGetUnionElementTypes, "LLVMGetUnionElementTypes");

  -- Operations on array, pointer, and vector types (sequence types)
   function LLVMArrayType (ElementType  : LLVMTypeRef; 
                           ElementCount : unsigned) return LLVMTypeRef;  -- Core.h:404
   pragma Import (C, LLVMArrayType, "LLVMArrayType");

   function LLVMPointerType (ElementType  : LLVMTypeRef; 
                             AddressSpace : unsigned) return LLVMTypeRef;  -- Core.h:405
   pragma Import (C, LLVMPointerType, "LLVMPointerType");

   function LLVMVectorType (ElementType  : LLVMTypeRef; 
                            ElementCount : unsigned) return LLVMTypeRef;  -- Core.h:406
   pragma Import (C, LLVMVectorType, "LLVMVectorType");

   function LLVMGetElementType (Ty : LLVMTypeRef) return LLVMTypeRef;  -- Core.h:408
   pragma Import (C, LLVMGetElementType, "LLVMGetElementType");

   function LLVMGetArrayLength (ArrayTy : LLVMTypeRef) return unsigned;  -- Core.h:409
   pragma Import (C, LLVMGetArrayLength, "LLVMGetArrayLength");

   function LLVMGetPointerAddressSpace (PointerTy : LLVMTypeRef) return unsigned;  -- Core.h:410
   pragma Import (C, LLVMGetPointerAddressSpace, "LLVMGetPointerAddressSpace");

   function LLVMGetVectorSize (VectorTy : LLVMTypeRef) return unsigned;  -- Core.h:411
   pragma Import (C, LLVMGetVectorSize, "LLVMGetVectorSize");

  -- Operations on other types
   function LLVMVoidTypeInContext (C : LLVMContextRef) return LLVMTypeRef;  -- Core.h:414
   pragma Import (C, LLVMVoidTypeInContext, "LLVMVoidTypeInContext");

   function LLVMLabelTypeInContext (C : LLVMContextRef) return LLVMTypeRef;  -- Core.h:415
   pragma Import (C, LLVMLabelTypeInContext, "LLVMLabelTypeInContext");

   function LLVMOpaqueTypeInContext (C : LLVMContextRef) return LLVMTypeRef;  -- Core.h:416
   pragma Import (C, LLVMOpaqueTypeInContext, "LLVMOpaqueTypeInContext");

   function LLVMVoidType return LLVMTypeRef;  -- Core.h:418
   pragma Import (C, LLVMVoidType, "LLVMVoidType");

   function LLVMLabelType return LLVMTypeRef;  -- Core.h:419
   pragma Import (C, LLVMLabelType, "LLVMLabelType");

   function LLVMOpaqueType return LLVMTypeRef;  -- Core.h:420
   pragma Import (C, LLVMOpaqueType, "LLVMOpaqueType");

  -- Operations on type handles
   function LLVMCreateTypeHandle (PotentiallyAbstractTy : LLVMTypeRef) return LLVMTypeHandleRef;  -- Core.h:423
   pragma Import (C, LLVMCreateTypeHandle, "LLVMCreateTypeHandle");

   procedure LLVMRefineType (AbstractTy : LLVMTypeRef; 
                             ConcreteTy : LLVMTypeRef);  -- Core.h:424
   pragma Import (C, LLVMRefineType, "LLVMRefineType");

   function LLVMResolveTypeHandle (TypeHandle : LLVMTypeHandleRef) return LLVMTypeRef;  -- Core.h:425
   pragma Import (C, LLVMResolveTypeHandle, "LLVMResolveTypeHandle");

   procedure LLVMDisposeTypeHandle (TypeHandle : LLVMTypeHandleRef);  -- Core.h:426
   pragma Import (C, LLVMDisposeTypeHandle, "LLVMDisposeTypeHandle");

  --===-- Values ------------------------------------------------------------===
  -- The bulk of LLVM's object model consists of values, which comprise a very
  -- * rich type hierarchy.
  --

  -- Operations on all values
   function LLVMTypeOf (arg1 : LLVMValueRef) return LLVMTypeRef;  -- Core.h:436
   pragma Import (C, LLVMTypeOf, "LLVMTypeOf");

   function LLVMGetValueName (arg1 : LLVMValueRef) return Interfaces.C.Strings.chars_ptr;  -- Core.h:437
   pragma Import (C, LLVMGetValueName, "LLVMGetValueName");

   procedure LLVMSetValueName (arg1 : LLVMValueRef; arg2 : Interfaces.C.Strings.chars_ptr);  -- Core.h:438
   pragma Import (C, LLVMSetValueName, "LLVMSetValueName");

   procedure LLVMDumpValue (arg1 : LLVMValueRef);  -- Core.h:439
   pragma Import (C, LLVMDumpValue, "LLVMDumpValue");

   procedure LLVMReplaceAllUsesWith (arg1 : LLVMValueRef; arg2 : LLVMValueRef);  -- Core.h:440
   pragma Import (C, LLVMReplaceAllUsesWith, "LLVMReplaceAllUsesWith");

   function LLVMHasMetadata (arg1 : LLVMValueRef) return int;  -- Core.h:441
   pragma Import (C, LLVMHasMetadata, "LLVMHasMetadata");

   function LLVMGetMetadata (arg1 : LLVMValueRef; arg2 : unsigned) return LLVMValueRef;  -- Core.h:442
   pragma Import (C, LLVMGetMetadata, "LLVMGetMetadata");

   procedure LLVMSetMetadata
     (arg1 : LLVMValueRef;
      arg2 : unsigned;
      arg3 : LLVMValueRef);  -- Core.h:443
   pragma Import (C, LLVMSetMetadata, "LLVMSetMetadata");

  -- Conversion functions. Return the input value if it is an instance of the
  --   specified class, otherwise NULL. See llvm::dyn_cast_or_null<>.

   function LLVMIsAArgument (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:448
   pragma Import (C, LLVMIsAArgument, "LLVMIsAArgument");

   function LLVMIsABasicBlock (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:449
   pragma Import (C, LLVMIsABasicBlock, "LLVMIsABasicBlock");

   function LLVMIsAInlineAsm (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:450
   pragma Import (C, LLVMIsAInlineAsm, "LLVMIsAInlineAsm");

   function LLVMIsAUser (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:451
   pragma Import (C, LLVMIsAUser, "LLVMIsAUser");

   function LLVMIsAConstant (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:452
   pragma Import (C, LLVMIsAConstant, "LLVMIsAConstant");

   function LLVMIsAConstantAggregateZero (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:453
   pragma Import (C, LLVMIsAConstantAggregateZero, "LLVMIsAConstantAggregateZero");

   function LLVMIsAConstantArray (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:454
   pragma Import (C, LLVMIsAConstantArray, "LLVMIsAConstantArray");

   function LLVMIsAConstantExpr (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:455
   pragma Import (C, LLVMIsAConstantExpr, "LLVMIsAConstantExpr");

   function LLVMIsAConstantFP (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:456
   pragma Import (C, LLVMIsAConstantFP, "LLVMIsAConstantFP");

   function LLVMIsAConstantInt (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:457
   pragma Import (C, LLVMIsAConstantInt, "LLVMIsAConstantInt");

   function LLVMIsAConstantPointerNull (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:458
   pragma Import (C, LLVMIsAConstantPointerNull, "LLVMIsAConstantPointerNull");

   function LLVMIsAConstantStruct (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:459
   pragma Import (C, LLVMIsAConstantStruct, "LLVMIsAConstantStruct");

   function LLVMIsAConstantVector (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:460
   pragma Import (C, LLVMIsAConstantVector, "LLVMIsAConstantVector");

   function LLVMIsAGlobalValue (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:461
   pragma Import (C, LLVMIsAGlobalValue, "LLVMIsAGlobalValue");

   function LLVMIsAFunction (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:462
   pragma Import (C, LLVMIsAFunction, "LLVMIsAFunction");

   function LLVMIsAGlobalAlias (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:463
   pragma Import (C, LLVMIsAGlobalAlias, "LLVMIsAGlobalAlias");

   function LLVMIsAGlobalVariable (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:464
   pragma Import (C, LLVMIsAGlobalVariable, "LLVMIsAGlobalVariable");

   function LLVMIsAUndefValue (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:465
   pragma Import (C, LLVMIsAUndefValue, "LLVMIsAUndefValue");

   function LLVMIsAInstruction (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:466
   pragma Import (C, LLVMIsAInstruction, "LLVMIsAInstruction");

   function LLVMIsABinaryOperator (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:467
   pragma Import (C, LLVMIsABinaryOperator, "LLVMIsABinaryOperator");

   function LLVMIsACallInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:468
   pragma Import (C, LLVMIsACallInst, "LLVMIsACallInst");

   function LLVMIsAIntrinsicInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:469
   pragma Import (C, LLVMIsAIntrinsicInst, "LLVMIsAIntrinsicInst");

   function LLVMIsADbgInfoIntrinsic (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:470
   pragma Import (C, LLVMIsADbgInfoIntrinsic, "LLVMIsADbgInfoIntrinsic");

   function LLVMIsADbgDeclareInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:471
   pragma Import (C, LLVMIsADbgDeclareInst, "LLVMIsADbgDeclareInst");

   function LLVMIsAEHSelectorInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:472
   pragma Import (C, LLVMIsAEHSelectorInst, "LLVMIsAEHSelectorInst");

   function LLVMIsAMemIntrinsic (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:473
   pragma Import (C, LLVMIsAMemIntrinsic, "LLVMIsAMemIntrinsic");

   function LLVMIsAMemCpyInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:474
   pragma Import (C, LLVMIsAMemCpyInst, "LLVMIsAMemCpyInst");

   function LLVMIsAMemMoveInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:475
   pragma Import (C, LLVMIsAMemMoveInst, "LLVMIsAMemMoveInst");

   function LLVMIsAMemSetInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:476
   pragma Import (C, LLVMIsAMemSetInst, "LLVMIsAMemSetInst");

   function LLVMIsACmpInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:477
   pragma Import (C, LLVMIsACmpInst, "LLVMIsACmpInst");

   function LLVMIsAFCmpInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:478
   pragma Import (C, LLVMIsAFCmpInst, "LLVMIsAFCmpInst");

   function LLVMIsAICmpInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:479
   pragma Import (C, LLVMIsAICmpInst, "LLVMIsAICmpInst");

   function LLVMIsAExtractElementInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:480
   pragma Import (C, LLVMIsAExtractElementInst, "LLVMIsAExtractElementInst");

   function LLVMIsAGetElementPtrInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:481
   pragma Import (C, LLVMIsAGetElementPtrInst, "LLVMIsAGetElementPtrInst");

   function LLVMIsAInsertElementInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:482
   pragma Import (C, LLVMIsAInsertElementInst, "LLVMIsAInsertElementInst");

   function LLVMIsAInsertValueInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:483
   pragma Import (C, LLVMIsAInsertValueInst, "LLVMIsAInsertValueInst");

   function LLVMIsAPHINode (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:484
   pragma Import (C, LLVMIsAPHINode, "LLVMIsAPHINode");

   function LLVMIsASelectInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:485
   pragma Import (C, LLVMIsASelectInst, "LLVMIsASelectInst");

   function LLVMIsAShuffleVectorInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:486
   pragma Import (C, LLVMIsAShuffleVectorInst, "LLVMIsAShuffleVectorInst");

   function LLVMIsAStoreInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:487
   pragma Import (C, LLVMIsAStoreInst, "LLVMIsAStoreInst");

   function LLVMIsATerminatorInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:488
   pragma Import (C, LLVMIsATerminatorInst, "LLVMIsATerminatorInst");

   function LLVMIsABranchInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:489
   pragma Import (C, LLVMIsABranchInst, "LLVMIsABranchInst");

   function LLVMIsAInvokeInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:490
   pragma Import (C, LLVMIsAInvokeInst, "LLVMIsAInvokeInst");

   function LLVMIsAReturnInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:491
   pragma Import (C, LLVMIsAReturnInst, "LLVMIsAReturnInst");

   function LLVMIsASwitchInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:492
   pragma Import (C, LLVMIsASwitchInst, "LLVMIsASwitchInst");

   function LLVMIsAUnreachableInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:493
   pragma Import (C, LLVMIsAUnreachableInst, "LLVMIsAUnreachableInst");

   function LLVMIsAUnwindInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:494
   pragma Import (C, LLVMIsAUnwindInst, "LLVMIsAUnwindInst");

   function LLVMIsAUnaryInstruction (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:495
   pragma Import (C, LLVMIsAUnaryInstruction, "LLVMIsAUnaryInstruction");

   function LLVMIsAAllocaInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:496
   pragma Import (C, LLVMIsAAllocaInst, "LLVMIsAAllocaInst");

   function LLVMIsACastInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:497
   pragma Import (C, LLVMIsACastInst, "LLVMIsACastInst");

   function LLVMIsABitCastInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:498
   pragma Import (C, LLVMIsABitCastInst, "LLVMIsABitCastInst");

   function LLVMIsAFPExtInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:499
   pragma Import (C, LLVMIsAFPExtInst, "LLVMIsAFPExtInst");

   function LLVMIsAFPToSIInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:500
   pragma Import (C, LLVMIsAFPToSIInst, "LLVMIsAFPToSIInst");

   function LLVMIsAFPToUIInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:501
   pragma Import (C, LLVMIsAFPToUIInst, "LLVMIsAFPToUIInst");

   function LLVMIsAFPTruncInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:502
   pragma Import (C, LLVMIsAFPTruncInst, "LLVMIsAFPTruncInst");

   function LLVMIsAIntToPtrInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:503
   pragma Import (C, LLVMIsAIntToPtrInst, "LLVMIsAIntToPtrInst");

   function LLVMIsAPtrToIntInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:504
   pragma Import (C, LLVMIsAPtrToIntInst, "LLVMIsAPtrToIntInst");

   function LLVMIsASExtInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:505
   pragma Import (C, LLVMIsASExtInst, "LLVMIsASExtInst");

   function LLVMIsASIToFPInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:506
   pragma Import (C, LLVMIsASIToFPInst, "LLVMIsASIToFPInst");

   function LLVMIsATruncInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:507
   pragma Import (C, LLVMIsATruncInst, "LLVMIsATruncInst");

   function LLVMIsAUIToFPInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:508
   pragma Import (C, LLVMIsAUIToFPInst, "LLVMIsAUIToFPInst");

   function LLVMIsAZExtInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:509
   pragma Import (C, LLVMIsAZExtInst, "LLVMIsAZExtInst");

   function LLVMIsAExtractValueInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:510
   pragma Import (C, LLVMIsAExtractValueInst, "LLVMIsAExtractValueInst");

   function LLVMIsALoadInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:511
   pragma Import (C, LLVMIsALoadInst, "LLVMIsALoadInst");

   function LLVMIsAVAArgInst (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:512
   pragma Import (C, LLVMIsAVAArgInst, "LLVMIsAVAArgInst");

  -- Operations on Uses
   function LLVMGetFirstUse (arg1 : LLVMValueRef) return LLVMUseRef;  -- Core.h:515
   pragma Import (C, LLVMGetFirstUse, "LLVMGetFirstUse");

   function LLVMGetNextUse (arg1 : LLVMUseRef) return LLVMUseRef;  -- Core.h:516
   pragma Import (C, LLVMGetNextUse, "LLVMGetNextUse");

   function LLVMGetUser (arg1 : LLVMUseRef) return LLVMValueRef;  -- Core.h:517
   pragma Import (C, LLVMGetUser, "LLVMGetUser");

   function LLVMGetUsedValue (arg1 : LLVMUseRef) return LLVMValueRef;  -- Core.h:518
   pragma Import (C, LLVMGetUsedValue, "LLVMGetUsedValue");

  -- Operations on Users
   function LLVMGetOperand (arg1 : LLVMValueRef; arg2 : unsigned) return LLVMValueRef;  -- Core.h:521
   pragma Import (C, LLVMGetOperand, "LLVMGetOperand");

  -- Operations on constants of any type
  -- all zeroes
   function LLVMConstNull (arg1 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:524
   pragma Import (C, LLVMConstNull, "LLVMConstNull");

  -- only for int/vector
   function LLVMConstAllOnes (arg1 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:525
   pragma Import (C, LLVMConstAllOnes, "LLVMConstAllOnes");

   function LLVMGetUndef (arg1 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:526
   pragma Import (C, LLVMGetUndef, "LLVMGetUndef");

   function LLVMIsConstant (arg1 : LLVMValueRef) return LLVMBool;  -- Core.h:527
   pragma Import (C, LLVMIsConstant, "LLVMIsConstant");

   function LLVMIsNull (arg1 : LLVMValueRef) return LLVMBool;  -- Core.h:528
   pragma Import (C, LLVMIsNull, "LLVMIsNull");

   function LLVMIsUndef (arg1 : LLVMValueRef) return LLVMBool;  -- Core.h:529
   pragma Import (C, LLVMIsUndef, "LLVMIsUndef");

   function LLVMConstPointerNull (arg1 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:530
   pragma Import (C, LLVMConstPointerNull, "LLVMConstPointerNull");

  -- Operations on metadata
   function LLVMMDStringInContext
     (arg1 : LLVMContextRef;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned) return LLVMValueRef;  -- Core.h:533
   pragma Import (C, LLVMMDStringInContext, "LLVMMDStringInContext");

   function LLVMMDString (arg1 : Interfaces.C.Strings.chars_ptr; arg2 : unsigned) return LLVMValueRef;  -- Core.h:535
   pragma Import (C, LLVMMDString, "LLVMMDString");

   function LLVMMDNodeInContext
     (arg1 : LLVMContextRef;
      arg2 : System.Address;
      arg3 : unsigned) return LLVMValueRef;  -- Core.h:536
   pragma Import (C, LLVMMDNodeInContext, "LLVMMDNodeInContext");

   function LLVMMDNode (arg1 : System.Address; arg2 : unsigned) return LLVMValueRef;  -- Core.h:538
   pragma Import (C, LLVMMDNode, "LLVMMDNode");

  -- Operations on scalar constants
   function LLVMConstInt
     (arg1 : LLVMTypeRef;
      arg2 : Extensions.unsigned_long_long;
      arg3 : LLVMBool) return LLVMValueRef;  -- Core.h:541
   pragma Import (C, LLVMConstInt, "LLVMConstInt");

   function LLVMConstIntOfString
     (arg1 : LLVMTypeRef;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : uint8_t) return LLVMValueRef;  -- Core.h:543
   pragma Import (C, LLVMConstIntOfString, "LLVMConstIntOfString");

   function LLVMConstIntOfStringAndSize
     (arg1 : LLVMTypeRef;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned;
      arg4 : uint8_t) return LLVMValueRef;  -- Core.h:545
   pragma Import (C, LLVMConstIntOfStringAndSize, "LLVMConstIntOfStringAndSize");

   function LLVMConstReal (arg1 : LLVMTypeRef; arg2 : double) return LLVMValueRef;  -- Core.h:547
   pragma Import (C, LLVMConstReal, "LLVMConstReal");

   function LLVMConstRealOfString (arg1 : LLVMTypeRef; arg2 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:548
   pragma Import (C, LLVMConstRealOfString, "LLVMConstRealOfString");

   function LLVMConstRealOfStringAndSize
     (arg1 : LLVMTypeRef;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned) return LLVMValueRef;  -- Core.h:549
   pragma Import (C, LLVMConstRealOfStringAndSize, "LLVMConstRealOfStringAndSize");

   function LLVMConstIntGetZExtValue (arg1 : LLVMValueRef) return Extensions.unsigned_long_long;  -- Core.h:551
   pragma Import (C, LLVMConstIntGetZExtValue, "LLVMConstIntGetZExtValue");

   function LLVMConstIntGetSExtValue (arg1 : LLVMValueRef) return Long_Long_Integer;  -- Core.h:552
   pragma Import (C, LLVMConstIntGetSExtValue, "LLVMConstIntGetSExtValue");

  -- Operations on composite constants
   function LLVMConstStringInContext
     (arg1 : LLVMContextRef;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : unsigned;
      arg4 : LLVMBool) return LLVMValueRef;  -- Core.h:556
   pragma Import (C, LLVMConstStringInContext, "LLVMConstStringInContext");

   function LLVMConstStructInContext
     (arg1 : LLVMContextRef;
      arg2 : System.Address;
      arg3 : unsigned;
      arg4 : LLVMBool) return LLVMValueRef;  -- Core.h:558
   pragma Import (C, LLVMConstStructInContext, "LLVMConstStructInContext");

   function LLVMConstString
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : unsigned;
      arg3 : LLVMBool) return LLVMValueRef;  -- Core.h:562
   pragma Import (C, LLVMConstString, "LLVMConstString");

   function LLVMConstArray
     (arg1 : LLVMTypeRef;
      arg2 : System.Address;
      arg3 : unsigned) return LLVMValueRef;  -- Core.h:564
   pragma Import (C, LLVMConstArray, "LLVMConstArray");

   function LLVMConstStruct
     (arg1 : System.Address;
      arg2 : unsigned;
      arg3 : LLVMBool) return LLVMValueRef;  -- Core.h:566
   pragma Import (C, LLVMConstStruct, "LLVMConstStruct");

   function LLVMConstVector (arg1 : System.Address; arg2 : unsigned) return LLVMValueRef;  -- Core.h:568
   pragma Import (C, LLVMConstVector, "LLVMConstVector");

   function LLVMConstUnion (arg1 : LLVMTypeRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:569
   pragma Import (C, LLVMConstUnion, "LLVMConstUnion");

  -- Constant expressions
   function LLVMGetConstOpcode (arg1 : LLVMValueRef) return LLVMOpcode;  -- Core.h:572
   pragma Import (C, LLVMGetConstOpcode, "LLVMGetConstOpcode");

   function LLVMAlignOf (arg1 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:573
   pragma Import (C, LLVMAlignOf, "LLVMAlignOf");

   function LLVMSizeOf (arg1 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:574
   pragma Import (C, LLVMSizeOf, "LLVMSizeOf");

   function LLVMConstNeg (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:575
   pragma Import (C, LLVMConstNeg, "LLVMConstNeg");

   function LLVMConstNSWNeg (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:576
   pragma Import (C, LLVMConstNSWNeg, "LLVMConstNSWNeg");

   function LLVMConstNUWNeg (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:577
   pragma Import (C, LLVMConstNUWNeg, "LLVMConstNUWNeg");

   function LLVMConstFNeg (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:578
   pragma Import (C, LLVMConstFNeg, "LLVMConstFNeg");

   function LLVMConstNot (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:579
   pragma Import (C, LLVMConstNot, "LLVMConstNot");

   function LLVMConstAdd (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:580
   pragma Import (C, LLVMConstAdd, "LLVMConstAdd");

   function LLVMConstNSWAdd (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:581
   pragma Import (C, LLVMConstNSWAdd, "LLVMConstNSWAdd");

   function LLVMConstNUWAdd (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:582
   pragma Import (C, LLVMConstNUWAdd, "LLVMConstNUWAdd");

   function LLVMConstFAdd (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:583
   pragma Import (C, LLVMConstFAdd, "LLVMConstFAdd");

   function LLVMConstSub (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:584
   pragma Import (C, LLVMConstSub, "LLVMConstSub");

   function LLVMConstNSWSub (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:585
   pragma Import (C, LLVMConstNSWSub, "LLVMConstNSWSub");

   function LLVMConstNUWSub (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:586
   pragma Import (C, LLVMConstNUWSub, "LLVMConstNUWSub");

   function LLVMConstFSub (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:587
   pragma Import (C, LLVMConstFSub, "LLVMConstFSub");

   function LLVMConstMul (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:588
   pragma Import (C, LLVMConstMul, "LLVMConstMul");

   function LLVMConstNSWMul (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:589
   pragma Import (C, LLVMConstNSWMul, "LLVMConstNSWMul");

   function LLVMConstNUWMul (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:590
   pragma Import (C, LLVMConstNUWMul, "LLVMConstNUWMul");

   function LLVMConstFMul (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:591
   pragma Import (C, LLVMConstFMul, "LLVMConstFMul");

   function LLVMConstUDiv (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:592
   pragma Import (C, LLVMConstUDiv, "LLVMConstUDiv");

   function LLVMConstSDiv (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:593
   pragma Import (C, LLVMConstSDiv, "LLVMConstSDiv");

   function LLVMConstExactSDiv (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:594
   pragma Import (C, LLVMConstExactSDiv, "LLVMConstExactSDiv");

   function LLVMConstFDiv (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:595
   pragma Import (C, LLVMConstFDiv, "LLVMConstFDiv");

   function LLVMConstURem (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:596
   pragma Import (C, LLVMConstURem, "LLVMConstURem");

   function LLVMConstSRem (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:597
   pragma Import (C, LLVMConstSRem, "LLVMConstSRem");

   function LLVMConstFRem (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:598
   pragma Import (C, LLVMConstFRem, "LLVMConstFRem");

   function LLVMConstAnd (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:599
   pragma Import (C, LLVMConstAnd, "LLVMConstAnd");

   function LLVMConstOr (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:600
   pragma Import (C, LLVMConstOr, "LLVMConstOr");

   function LLVMConstXor (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:601
   pragma Import (C, LLVMConstXor, "LLVMConstXor");

   function LLVMConstICmp
     (arg1 : LLVMIntPredicate;
      arg2 : LLVMValueRef;
      arg3 : LLVMValueRef) return LLVMValueRef;  -- Core.h:602
   pragma Import (C, LLVMConstICmp, "LLVMConstICmp");

   function LLVMConstFCmp
     (arg1 : LLVMRealPredicate;
      arg2 : LLVMValueRef;
      arg3 : LLVMValueRef) return LLVMValueRef;  -- Core.h:604
   pragma Import (C, LLVMConstFCmp, "LLVMConstFCmp");

   function LLVMConstShl (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:606
   pragma Import (C, LLVMConstShl, "LLVMConstShl");

   function LLVMConstLShr (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:607
   pragma Import (C, LLVMConstLShr, "LLVMConstLShr");

   function LLVMConstAShr (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:608
   pragma Import (C, LLVMConstAShr, "LLVMConstAShr");

   function LLVMConstGEP
     (arg1 : LLVMValueRef;
      arg2 : System.Address;
      arg3 : unsigned) return LLVMValueRef;  -- Core.h:609
   pragma Import (C, LLVMConstGEP, "LLVMConstGEP");

   function LLVMConstInBoundsGEP
     (arg1 : LLVMValueRef;
      arg2 : System.Address;
      arg3 : unsigned) return LLVMValueRef;  -- Core.h:611
   pragma Import (C, LLVMConstInBoundsGEP, "LLVMConstInBoundsGEP");

   function LLVMConstTrunc (arg1 : LLVMValueRef; arg2 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:614
   pragma Import (C, LLVMConstTrunc, "LLVMConstTrunc");

   function LLVMConstSExt (arg1 : LLVMValueRef; arg2 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:615
   pragma Import (C, LLVMConstSExt, "LLVMConstSExt");

   function LLVMConstZExt (arg1 : LLVMValueRef; arg2 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:616
   pragma Import (C, LLVMConstZExt, "LLVMConstZExt");

   function LLVMConstFPTrunc (arg1 : LLVMValueRef; arg2 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:617
   pragma Import (C, LLVMConstFPTrunc, "LLVMConstFPTrunc");

   function LLVMConstFPExt (arg1 : LLVMValueRef; arg2 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:618
   pragma Import (C, LLVMConstFPExt, "LLVMConstFPExt");

   function LLVMConstUIToFP (arg1 : LLVMValueRef; arg2 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:619
   pragma Import (C, LLVMConstUIToFP, "LLVMConstUIToFP");

   function LLVMConstSIToFP (arg1 : LLVMValueRef; arg2 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:620
   pragma Import (C, LLVMConstSIToFP, "LLVMConstSIToFP");

   function LLVMConstFPToUI (arg1 : LLVMValueRef; arg2 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:621
   pragma Import (C, LLVMConstFPToUI, "LLVMConstFPToUI");

   function LLVMConstFPToSI (arg1 : LLVMValueRef; arg2 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:622
   pragma Import (C, LLVMConstFPToSI, "LLVMConstFPToSI");

   function LLVMConstPtrToInt (arg1 : LLVMValueRef; arg2 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:623
   pragma Import (C, LLVMConstPtrToInt, "LLVMConstPtrToInt");

   function LLVMConstIntToPtr (arg1 : LLVMValueRef; arg2 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:624
   pragma Import (C, LLVMConstIntToPtr, "LLVMConstIntToPtr");

   function LLVMConstBitCast (arg1 : LLVMValueRef; arg2 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:625
   pragma Import (C, LLVMConstBitCast, "LLVMConstBitCast");

   function LLVMConstZExtOrBitCast (arg1 : LLVMValueRef; arg2 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:626
   pragma Import (C, LLVMConstZExtOrBitCast, "LLVMConstZExtOrBitCast");

   function LLVMConstSExtOrBitCast (arg1 : LLVMValueRef; arg2 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:628
   pragma Import (C, LLVMConstSExtOrBitCast, "LLVMConstSExtOrBitCast");

   function LLVMConstTruncOrBitCast (arg1 : LLVMValueRef; arg2 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:630
   pragma Import (C, LLVMConstTruncOrBitCast, "LLVMConstTruncOrBitCast");

   function LLVMConstPointerCast (arg1 : LLVMValueRef; arg2 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:632
   pragma Import (C, LLVMConstPointerCast, "LLVMConstPointerCast");

   function LLVMConstIntCast
     (arg1 : LLVMValueRef;
      arg2 : LLVMTypeRef;
      arg3 : LLVMBool) return LLVMValueRef;  -- Core.h:634
   pragma Import (C, LLVMConstIntCast, "LLVMConstIntCast");

   function LLVMConstFPCast (arg1 : LLVMValueRef; arg2 : LLVMTypeRef) return LLVMValueRef;  -- Core.h:636
   pragma Import (C, LLVMConstFPCast, "LLVMConstFPCast");

   function LLVMConstSelect
     (arg1 : LLVMValueRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMValueRef) return LLVMValueRef;  -- Core.h:637
   pragma Import (C, LLVMConstSelect, "LLVMConstSelect");

   function LLVMConstExtractElement (arg1 : LLVMValueRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:640
   pragma Import (C, LLVMConstExtractElement, "LLVMConstExtractElement");

   function LLVMConstInsertElement
     (arg1 : LLVMValueRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMValueRef) return LLVMValueRef;  -- Core.h:642
   pragma Import (C, LLVMConstInsertElement, "LLVMConstInsertElement");

   function LLVMConstShuffleVector
     (arg1 : LLVMValueRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMValueRef) return LLVMValueRef;  -- Core.h:645
   pragma Import (C, LLVMConstShuffleVector, "LLVMConstShuffleVector");

   function LLVMConstExtractValue
     (arg1 : LLVMValueRef;
      arg2 : access unsigned;
      arg3 : unsigned) return LLVMValueRef;  -- Core.h:648
   pragma Import (C, LLVMConstExtractValue, "LLVMConstExtractValue");

   function LLVMConstInsertValue
     (arg1 : LLVMValueRef;
      arg2 : LLVMValueRef;
      arg3 : access unsigned;
      arg4 : unsigned) return LLVMValueRef;  -- Core.h:650
   pragma Import (C, LLVMConstInsertValue, "LLVMConstInsertValue");

   function LLVMConstInlineAsm
     (arg1 : LLVMTypeRef;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : Interfaces.C.Strings.chars_ptr;
      arg4 : LLVMBool;
      arg5 : LLVMBool) return LLVMValueRef;  -- Core.h:653
   pragma Import (C, LLVMConstInlineAsm, "LLVMConstInlineAsm");

   function LLVMBlockAddress (arg1 : LLVMValueRef; arg2 : LLVMBasicBlockRef) return LLVMValueRef;  -- Core.h:656
   pragma Import (C, LLVMBlockAddress, "LLVMBlockAddress");

  -- Operations on global variables, functions, and aliases (globals)
   function LLVMGetGlobalParent (arg1 : LLVMValueRef) return LLVMModuleRef;  -- Core.h:659
   pragma Import (C, LLVMGetGlobalParent, "LLVMGetGlobalParent");

   function LLVMIsDeclaration (arg1 : LLVMValueRef) return LLVMBool;  -- Core.h:660
   pragma Import (C, LLVMIsDeclaration, "LLVMIsDeclaration");

   function LLVMGetLinkage (arg1 : LLVMValueRef) return LLVMLinkage;  -- Core.h:661
   pragma Import (C, LLVMGetLinkage, "LLVMGetLinkage");

   procedure LLVMSetLinkage (arg1 : LLVMValueRef; arg2 : LLVMLinkage);  -- Core.h:662
   pragma Import (C, LLVMSetLinkage, "LLVMSetLinkage");

   function LLVMGetSection (arg1 : LLVMValueRef) return Interfaces.C.Strings.chars_ptr;  -- Core.h:663
   pragma Import (C, LLVMGetSection, "LLVMGetSection");

   procedure LLVMSetSection (arg1 : LLVMValueRef; arg2 : Interfaces.C.Strings.chars_ptr);  -- Core.h:664
   pragma Import (C, LLVMSetSection, "LLVMSetSection");

   function LLVMGetVisibility (arg1 : LLVMValueRef) return LLVMVisibility;  -- Core.h:665
   pragma Import (C, LLVMGetVisibility, "LLVMGetVisibility");

   procedure LLVMSetVisibility (arg1 : LLVMValueRef; arg2 : LLVMVisibility);  -- Core.h:666
   pragma Import (C, LLVMSetVisibility, "LLVMSetVisibility");

   function LLVMGetAlignment (arg1 : LLVMValueRef) return unsigned;  -- Core.h:667
   pragma Import (C, LLVMGetAlignment, "LLVMGetAlignment");

   procedure LLVMSetAlignment (arg1 : LLVMValueRef; arg2 : unsigned);  -- Core.h:668
   pragma Import (C, LLVMSetAlignment, "LLVMSetAlignment");

  -- Operations on global variables
   function LLVMAddGlobal
     (arg1 : LLVMModuleRef;
      arg2 : LLVMTypeRef;
      arg3 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:671
   pragma Import (C, LLVMAddGlobal, "LLVMAddGlobal");

   function LLVMAddGlobalInAddressSpace
     (arg1 : LLVMModuleRef;
      arg2 : LLVMTypeRef;
      arg3 : Interfaces.C.Strings.chars_ptr;
      arg4 : unsigned) return LLVMValueRef;  -- Core.h:672
   pragma Import (C, LLVMAddGlobalInAddressSpace, "LLVMAddGlobalInAddressSpace");

   function LLVMGetNamedGlobal (arg1 : LLVMModuleRef; arg2 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:675
   pragma Import (C, LLVMGetNamedGlobal, "LLVMGetNamedGlobal");

   function LLVMGetFirstGlobal (arg1 : LLVMModuleRef) return LLVMValueRef;  -- Core.h:676
   pragma Import (C, LLVMGetFirstGlobal, "LLVMGetFirstGlobal");

   function LLVMGetLastGlobal (arg1 : LLVMModuleRef) return LLVMValueRef;  -- Core.h:677
   pragma Import (C, LLVMGetLastGlobal, "LLVMGetLastGlobal");

   function LLVMGetNextGlobal (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:678
   pragma Import (C, LLVMGetNextGlobal, "LLVMGetNextGlobal");

   function LLVMGetPreviousGlobal (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:679
   pragma Import (C, LLVMGetPreviousGlobal, "LLVMGetPreviousGlobal");

   procedure LLVMDeleteGlobal (arg1 : LLVMValueRef);  -- Core.h:680
   pragma Import (C, LLVMDeleteGlobal, "LLVMDeleteGlobal");

   function LLVMGetInitializer (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:681
   pragma Import (C, LLVMGetInitializer, "LLVMGetInitializer");

   procedure LLVMSetInitializer (arg1 : LLVMValueRef; arg2 : LLVMValueRef);  -- Core.h:682
   pragma Import (C, LLVMSetInitializer, "LLVMSetInitializer");

   function LLVMIsThreadLocal (arg1 : LLVMValueRef) return LLVMBool;  -- Core.h:683
   pragma Import (C, LLVMIsThreadLocal, "LLVMIsThreadLocal");

   procedure LLVMSetThreadLocal (arg1 : LLVMValueRef; arg2 : LLVMBool);  -- Core.h:684
   pragma Import (C, LLVMSetThreadLocal, "LLVMSetThreadLocal");

   function LLVMIsGlobalConstant (arg1 : LLVMValueRef) return LLVMBool;  -- Core.h:685
   pragma Import (C, LLVMIsGlobalConstant, "LLVMIsGlobalConstant");

   procedure LLVMSetGlobalConstant (arg1 : LLVMValueRef; arg2 : LLVMBool);  -- Core.h:686
   pragma Import (C, LLVMSetGlobalConstant, "LLVMSetGlobalConstant");

  -- Operations on aliases
   function LLVMAddAlias
     (arg1 : LLVMModuleRef;
      arg2 : LLVMTypeRef;
      arg3 : LLVMValueRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:689
   pragma Import (C, LLVMAddAlias, "LLVMAddAlias");

  -- Operations on functions
   function LLVMAddFunction
     (M          : LLVMModuleRef;
      Name       : Interfaces.C.Strings.chars_ptr;
      FunctionTy : LLVMTypeRef) return LLVMValueRef;  -- Core.h:693
   pragma Import (C, LLVMAddFunction, "LLVMAddFunction");

   function LLVMGetNamedFunction (arg1 : LLVMModuleRef; arg2 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:695
   pragma Import (C, LLVMGetNamedFunction, "LLVMGetNamedFunction");

   function LLVMGetFirstFunction (arg1 : LLVMModuleRef) return LLVMValueRef;  -- Core.h:696
   pragma Import (C, LLVMGetFirstFunction, "LLVMGetFirstFunction");

   function LLVMGetLastFunction (arg1 : LLVMModuleRef) return LLVMValueRef;  -- Core.h:697
   pragma Import (C, LLVMGetLastFunction, "LLVMGetLastFunction");

   function LLVMGetNextFunction (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:698
   pragma Import (C, LLVMGetNextFunction, "LLVMGetNextFunction");

   function LLVMGetPreviousFunction (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:699
   pragma Import (C, LLVMGetPreviousFunction, "LLVMGetPreviousFunction");

   procedure LLVMDeleteFunction (arg1 : LLVMValueRef);  -- Core.h:700
   pragma Import (C, LLVMDeleteFunction, "LLVMDeleteFunction");

   function LLVMGetIntrinsicID (arg1 : LLVMValueRef) return unsigned;  -- Core.h:701
   pragma Import (C, LLVMGetIntrinsicID, "LLVMGetIntrinsicID");

   function LLVMGetFunctionCallConv (arg1 : LLVMValueRef) return unsigned;  -- Core.h:702
   pragma Import (C, LLVMGetFunctionCallConv, "LLVMGetFunctionCallConv");

   procedure LLVMSetFunctionCallConv (arg1 : LLVMValueRef; arg2 : unsigned);  -- Core.h:703
   pragma Import (C, LLVMSetFunctionCallConv, "LLVMSetFunctionCallConv");

   function LLVMGetGC (arg1 : LLVMValueRef) return Interfaces.C.Strings.chars_ptr;  -- Core.h:704
   pragma Import (C, LLVMGetGC, "LLVMGetGC");

   procedure LLVMSetGC (arg1 : LLVMValueRef; arg2 : Interfaces.C.Strings.chars_ptr);  -- Core.h:705
   pragma Import (C, LLVMSetGC, "LLVMSetGC");

   procedure LLVMAddFunctionAttr (arg1 : LLVMValueRef; arg2 : LLVMAttribute);  -- Core.h:706
   pragma Import (C, LLVMAddFunctionAttr, "LLVMAddFunctionAttr");

   function LLVMGetFunctionAttr (arg1 : LLVMValueRef) return LLVMAttribute;  -- Core.h:707
   pragma Import (C, LLVMGetFunctionAttr, "LLVMGetFunctionAttr");

   procedure LLVMRemoveFunctionAttr (arg1 : LLVMValueRef; arg2 : LLVMAttribute);  -- Core.h:708
   pragma Import (C, LLVMRemoveFunctionAttr, "LLVMRemoveFunctionAttr");

  -- Operations on parameters
   function LLVMCountParams (arg1 : LLVMValueRef) return unsigned;  -- Core.h:711
   pragma Import (C, LLVMCountParams, "LLVMCountParams");

   procedure LLVMGetParams (arg1 : LLVMValueRef; arg2 : System.Address);  -- Core.h:712
   pragma Import (C, LLVMGetParams, "LLVMGetParams");

   function LLVMGetParam (arg1 : LLVMValueRef; arg2 : unsigned) return LLVMValueRef;  -- Core.h:713
   pragma Import (C, LLVMGetParam, "LLVMGetParam");

   function LLVMGetParamParent (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:714
   pragma Import (C, LLVMGetParamParent, "LLVMGetParamParent");

   function LLVMGetFirstParam (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:715
   pragma Import (C, LLVMGetFirstParam, "LLVMGetFirstParam");

   function LLVMGetLastParam (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:716
   pragma Import (C, LLVMGetLastParam, "LLVMGetLastParam");

   function LLVMGetNextParam (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:717
   pragma Import (C, LLVMGetNextParam, "LLVMGetNextParam");

   function LLVMGetPreviousParam (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:718
   pragma Import (C, LLVMGetPreviousParam, "LLVMGetPreviousParam");

   procedure LLVMAddAttribute (arg1 : LLVMValueRef; arg2 : LLVMAttribute);  -- Core.h:719
   pragma Import (C, LLVMAddAttribute, "LLVMAddAttribute");

   procedure LLVMRemoveAttribute (arg1 : LLVMValueRef; arg2 : LLVMAttribute);  -- Core.h:720
   pragma Import (C, LLVMRemoveAttribute, "LLVMRemoveAttribute");

   function LLVMGetAttribute (arg1 : LLVMValueRef) return LLVMAttribute;  -- Core.h:721
   pragma Import (C, LLVMGetAttribute, "LLVMGetAttribute");

   procedure LLVMSetParamAlignment (arg1 : LLVMValueRef; arg2 : unsigned);  -- Core.h:722
   pragma Import (C, LLVMSetParamAlignment, "LLVMSetParamAlignment");

  -- Operations on basic blocks
   function LLVMBasicBlockAsValue (arg1 : LLVMBasicBlockRef) return LLVMValueRef;  -- Core.h:725
   pragma Import (C, LLVMBasicBlockAsValue, "LLVMBasicBlockAsValue");

   function LLVMValueIsBasicBlock (arg1 : LLVMValueRef) return LLVMBool;  -- Core.h:726
   pragma Import (C, LLVMValueIsBasicBlock, "LLVMValueIsBasicBlock");

   function LLVMValueAsBasicBlock (arg1 : LLVMValueRef) return LLVMBasicBlockRef;  -- Core.h:727
   pragma Import (C, LLVMValueAsBasicBlock, "LLVMValueAsBasicBlock");

   function LLVMGetBasicBlockParent (arg1 : LLVMBasicBlockRef) return LLVMValueRef;  -- Core.h:728
   pragma Import (C, LLVMGetBasicBlockParent, "LLVMGetBasicBlockParent");

   function LLVMCountBasicBlocks (arg1 : LLVMValueRef) return unsigned;  -- Core.h:729
   pragma Import (C, LLVMCountBasicBlocks, "LLVMCountBasicBlocks");

   procedure LLVMGetBasicBlocks (arg1 : LLVMValueRef; arg2 : System.Address);  -- Core.h:730
   pragma Import (C, LLVMGetBasicBlocks, "LLVMGetBasicBlocks");

   function LLVMGetFirstBasicBlock (arg1 : LLVMValueRef) return LLVMBasicBlockRef;  -- Core.h:731
   pragma Import (C, LLVMGetFirstBasicBlock, "LLVMGetFirstBasicBlock");

   function LLVMGetLastBasicBlock (arg1 : LLVMValueRef) return LLVMBasicBlockRef;  -- Core.h:732
   pragma Import (C, LLVMGetLastBasicBlock, "LLVMGetLastBasicBlock");

   function LLVMGetNextBasicBlock (arg1 : LLVMBasicBlockRef) return LLVMBasicBlockRef;  -- Core.h:733
   pragma Import (C, LLVMGetNextBasicBlock, "LLVMGetNextBasicBlock");

   function LLVMGetPreviousBasicBlock (arg1 : LLVMBasicBlockRef) return LLVMBasicBlockRef;  -- Core.h:734
   pragma Import (C, LLVMGetPreviousBasicBlock, "LLVMGetPreviousBasicBlock");

   function LLVMGetEntryBasicBlock (arg1 : LLVMValueRef) return LLVMBasicBlockRef;  -- Core.h:735
   pragma Import (C, LLVMGetEntryBasicBlock, "LLVMGetEntryBasicBlock");

   function LLVMAppendBasicBlockInContext
     (C    : LLVMContextRef;
      Fn   : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMBasicBlockRef;  -- Core.h:737
   pragma Import (C, LLVMAppendBasicBlockInContext, "LLVMAppendBasicBlockInContext");

   function LLVMInsertBasicBlockInContext
     (C    : LLVMContextRef;
      BB   : LLVMBasicBlockRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMBasicBlockRef;  -- Core.h:740
   pragma Import (C, LLVMInsertBasicBlockInContext, "LLVMInsertBasicBlockInContext");

   function LLVMAppendBasicBlock (arg1 : LLVMValueRef; arg2 : Interfaces.C.Strings.chars_ptr) return LLVMBasicBlockRef;  -- Core.h:744
   pragma Import (C, LLVMAppendBasicBlock, "LLVMAppendBasicBlock");

   function LLVMInsertBasicBlock (arg1 : LLVMBasicBlockRef; arg2 : Interfaces.C.Strings.chars_ptr) return LLVMBasicBlockRef;  -- Core.h:745
   pragma Import (C, LLVMInsertBasicBlock, "LLVMInsertBasicBlock");

   procedure LLVMDeleteBasicBlock (arg1 : LLVMBasicBlockRef);  -- Core.h:747
   pragma Import (C, LLVMDeleteBasicBlock, "LLVMDeleteBasicBlock");

  -- Operations on instructions
   function LLVMGetInstructionParent (arg1 : LLVMValueRef) return LLVMBasicBlockRef;  -- Core.h:750
   pragma Import (C, LLVMGetInstructionParent, "LLVMGetInstructionParent");

   function LLVMGetFirstInstruction (arg1 : LLVMBasicBlockRef) return LLVMValueRef;  -- Core.h:751
   pragma Import (C, LLVMGetFirstInstruction, "LLVMGetFirstInstruction");

   function LLVMGetLastInstruction (arg1 : LLVMBasicBlockRef) return LLVMValueRef;  -- Core.h:752
   pragma Import (C, LLVMGetLastInstruction, "LLVMGetLastInstruction");

   function LLVMGetNextInstruction (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:753
   pragma Import (C, LLVMGetNextInstruction, "LLVMGetNextInstruction");

   function LLVMGetPreviousInstruction (arg1 : LLVMValueRef) return LLVMValueRef;  -- Core.h:754
   pragma Import (C, LLVMGetPreviousInstruction, "LLVMGetPreviousInstruction");

  -- Operations on call sites
   procedure LLVMSetInstructionCallConv (arg1 : LLVMValueRef; arg2 : unsigned);  -- Core.h:757
   pragma Import (C, LLVMSetInstructionCallConv, "LLVMSetInstructionCallConv");

   function LLVMGetInstructionCallConv (arg1 : LLVMValueRef) return unsigned;  -- Core.h:758
   pragma Import (C, LLVMGetInstructionCallConv, "LLVMGetInstructionCallConv");

   procedure LLVMAddInstrAttribute
     (arg1 : LLVMValueRef;
      arg2 : unsigned;
      arg3 : LLVMAttribute);  -- Core.h:759
   pragma Import (C, LLVMAddInstrAttribute, "LLVMAddInstrAttribute");

   procedure LLVMRemoveInstrAttribute
     (arg1 : LLVMValueRef;
      arg2 : unsigned;
      arg3 : LLVMAttribute);  -- Core.h:760
   pragma Import (C, LLVMRemoveInstrAttribute, "LLVMRemoveInstrAttribute");

   procedure LLVMSetInstrParamAlignment
     (arg1 : LLVMValueRef;
      arg2 : unsigned;
      arg3 : unsigned);  -- Core.h:762
   pragma Import (C, LLVMSetInstrParamAlignment, "LLVMSetInstrParamAlignment");

  -- Operations on call instructions (only)
   function LLVMIsTailCall (arg1 : LLVMValueRef) return LLVMBool;  -- Core.h:766
   pragma Import (C, LLVMIsTailCall, "LLVMIsTailCall");

   procedure LLVMSetTailCall (arg1 : LLVMValueRef; arg2 : LLVMBool);  -- Core.h:767
   pragma Import (C, LLVMSetTailCall, "LLVMSetTailCall");

  -- Operations on phi nodes
   procedure LLVMAddIncoming
     (arg1 : LLVMValueRef;
      arg2 : System.Address;
      arg3 : System.Address;
      arg4 : unsigned);  -- Core.h:770
   pragma Import (C, LLVMAddIncoming, "LLVMAddIncoming");

   function LLVMCountIncoming (arg1 : LLVMValueRef) return unsigned;  -- Core.h:772
   pragma Import (C, LLVMCountIncoming, "LLVMCountIncoming");

   function LLVMGetIncomingValue (arg1 : LLVMValueRef; arg2 : unsigned) return LLVMValueRef;  -- Core.h:773
   pragma Import (C, LLVMGetIncomingValue, "LLVMGetIncomingValue");

   function LLVMGetIncomingBlock (arg1 : LLVMValueRef; arg2 : unsigned) return LLVMBasicBlockRef;  -- Core.h:774
   pragma Import (C, LLVMGetIncomingBlock, "LLVMGetIncomingBlock");

  --===-- Instruction builders ----------------------------------------------===
  -- An instruction builder represents a point within a basic block, and is the
  -- * exclusive means of building instructions using the C interface.
  --

   function LLVMCreateBuilderInContext (arg1 : LLVMContextRef) return LLVMBuilderRef;  -- Core.h:782
   pragma Import (C, LLVMCreateBuilderInContext, "LLVMCreateBuilderInContext");

   function LLVMCreateBuilder return LLVMBuilderRef;  -- Core.h:783
   pragma Import (C, LLVMCreateBuilder, "LLVMCreateBuilder");

   procedure LLVMPositionBuilder
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMBasicBlockRef;
      arg3 : LLVMValueRef);  -- Core.h:784
   pragma Import (C, LLVMPositionBuilder, "LLVMPositionBuilder");

   procedure LLVMPositionBuilderBefore (arg1 : LLVMBuilderRef; arg2 : LLVMValueRef);  -- Core.h:786
   pragma Import (C, LLVMPositionBuilderBefore, "LLVMPositionBuilderBefore");

   procedure LLVMPositionBuilderAtEnd (arg1 : LLVMBuilderRef; arg2 : LLVMBasicBlockRef);  -- Core.h:787
   pragma Import (C, LLVMPositionBuilderAtEnd, "LLVMPositionBuilderAtEnd");

   function LLVMGetInsertBlock (arg1 : LLVMBuilderRef) return LLVMBasicBlockRef;  -- Core.h:788
   pragma Import (C, LLVMGetInsertBlock, "LLVMGetInsertBlock");

   procedure LLVMClearInsertionPosition (arg1 : LLVMBuilderRef);  -- Core.h:789
   pragma Import (C, LLVMClearInsertionPosition, "LLVMClearInsertionPosition");

   procedure LLVMInsertIntoBuilder (arg1 : LLVMBuilderRef; arg2 : LLVMValueRef);  -- Core.h:790
   pragma Import (C, LLVMInsertIntoBuilder, "LLVMInsertIntoBuilder");

   procedure LLVMInsertIntoBuilderWithName
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : Interfaces.C.Strings.chars_ptr);  -- Core.h:791
   pragma Import (C, LLVMInsertIntoBuilderWithName, "LLVMInsertIntoBuilderWithName");

   procedure LLVMDisposeBuilder (arg1 : LLVMBuilderRef);  -- Core.h:793
   pragma Import (C, LLVMDisposeBuilder, "LLVMDisposeBuilder");

  -- Metadata
   procedure LLVMSetCurrentDebugLocation (arg1 : LLVMBuilderRef; arg2 : LLVMValueRef);  -- Core.h:796
   pragma Import (C, LLVMSetCurrentDebugLocation, "LLVMSetCurrentDebugLocation");

   function LLVMGetCurrentDebugLocation (arg1 : LLVMBuilderRef) return LLVMValueRef;  -- Core.h:797
   pragma Import (C, LLVMGetCurrentDebugLocation, "LLVMGetCurrentDebugLocation");

   procedure LLVMSetInstDebugLocation (arg1 : LLVMBuilderRef; arg2 : LLVMValueRef);  -- Core.h:798
   pragma Import (C, LLVMSetInstDebugLocation, "LLVMSetInstDebugLocation");

  -- Terminators
   function LLVMBuildRetVoid (arg1 : LLVMBuilderRef) return LLVMValueRef;  -- Core.h:801
   pragma Import (C, LLVMBuildRetVoid, "LLVMBuildRetVoid");

   function LLVMBuildRet (arg1 : LLVMBuilderRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:802
   pragma Import (C, LLVMBuildRet, "LLVMBuildRet");

   function LLVMBuildAggregateRet
     (arg1 : LLVMBuilderRef;
      arg2 : System.Address;
      arg3 : unsigned) return LLVMValueRef;  -- Core.h:803
   pragma Import (C, LLVMBuildAggregateRet, "LLVMBuildAggregateRet");

   function LLVMBuildBr (arg1 : LLVMBuilderRef; arg2 : LLVMBasicBlockRef) return LLVMValueRef;  -- Core.h:805
   pragma Import (C, LLVMBuildBr, "LLVMBuildBr");

   function LLVMBuildCondBr
     (B     : LLVMBuilderRef;
      Bif   : LLVMValueRef;
      Bthen : LLVMBasicBlockRef;
      Belse : LLVMBasicBlockRef) return LLVMValueRef;  -- Core.h:806
   pragma Import (C, LLVMBuildCondBr, "LLVMBuildCondBr");

   function LLVMBuildSwitch
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMBasicBlockRef;
      arg4 : unsigned) return LLVMValueRef;  -- Core.h:808
   pragma Import (C, LLVMBuildSwitch, "LLVMBuildSwitch");

   function LLVMBuildIndirectBr
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : unsigned) return LLVMValueRef;  -- Core.h:810
   pragma Import (C, LLVMBuildIndirectBr, "LLVMBuildIndirectBr");

   function LLVMBuildInvoke
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : System.Address;
      arg4 : unsigned;
      arg5 : LLVMBasicBlockRef;
      arg6 : LLVMBasicBlockRef;
      arg7 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:812
   pragma Import (C, LLVMBuildInvoke, "LLVMBuildInvoke");

   function LLVMBuildUnwind (arg1 : LLVMBuilderRef) return LLVMValueRef;  -- Core.h:816
   pragma Import (C, LLVMBuildUnwind, "LLVMBuildUnwind");

   function LLVMBuildUnreachable (arg1 : LLVMBuilderRef) return LLVMValueRef;  -- Core.h:817
   pragma Import (C, LLVMBuildUnreachable, "LLVMBuildUnreachable");

  -- Add a case to the switch instruction
   procedure LLVMAddCase
     (arg1 : LLVMValueRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMBasicBlockRef);  -- Core.h:820
   pragma Import (C, LLVMAddCase, "LLVMAddCase");

  -- Add a destination to the indirectbr instruction
   procedure LLVMAddDestination (arg1 : LLVMValueRef; arg2 : LLVMBasicBlockRef);  -- Core.h:824
   pragma Import (C, LLVMAddDestination, "LLVMAddDestination");

  -- Arithmetic
   function LLVMBuildAdd
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:827
   pragma Import (C, LLVMBuildAdd, "LLVMBuildAdd");

   function LLVMBuildNSWAdd
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:829
   pragma Import (C, LLVMBuildNSWAdd, "LLVMBuildNSWAdd");

   function LLVMBuildNUWAdd
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:831
   pragma Import (C, LLVMBuildNUWAdd, "LLVMBuildNUWAdd");

   function LLVMBuildFAdd
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:833
   pragma Import (C, LLVMBuildFAdd, "LLVMBuildFAdd");

   function LLVMBuildSub
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:835
   pragma Import (C, LLVMBuildSub, "LLVMBuildSub");

   function LLVMBuildNSWSub
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:837
   pragma Import (C, LLVMBuildNSWSub, "LLVMBuildNSWSub");

   function LLVMBuildNUWSub
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:839
   pragma Import (C, LLVMBuildNUWSub, "LLVMBuildNUWSub");

   function LLVMBuildFSub
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:841
   pragma Import (C, LLVMBuildFSub, "LLVMBuildFSub");

   function LLVMBuildMul
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:843
   pragma Import (C, LLVMBuildMul, "LLVMBuildMul");

   function LLVMBuildNSWMul
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:845
   pragma Import (C, LLVMBuildNSWMul, "LLVMBuildNSWMul");

   function LLVMBuildNUWMul
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:847
   pragma Import (C, LLVMBuildNUWMul, "LLVMBuildNUWMul");

   function LLVMBuildFMul
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:849
   pragma Import (C, LLVMBuildFMul, "LLVMBuildFMul");

   function LLVMBuildUDiv
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:851
   pragma Import (C, LLVMBuildUDiv, "LLVMBuildUDiv");

   function LLVMBuildSDiv
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:853
   pragma Import (C, LLVMBuildSDiv, "LLVMBuildSDiv");

   function LLVMBuildExactSDiv
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:855
   pragma Import (C, LLVMBuildExactSDiv, "LLVMBuildExactSDiv");

   function LLVMBuildFDiv
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:857
   pragma Import (C, LLVMBuildFDiv, "LLVMBuildFDiv");

   function LLVMBuildURem
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:859
   pragma Import (C, LLVMBuildURem, "LLVMBuildURem");

   function LLVMBuildSRem
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:861
   pragma Import (C, LLVMBuildSRem, "LLVMBuildSRem");

   function LLVMBuildFRem
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:863
   pragma Import (C, LLVMBuildFRem, "LLVMBuildFRem");

   function LLVMBuildShl
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:865
   pragma Import (C, LLVMBuildShl, "LLVMBuildShl");

   function LLVMBuildLShr
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:867
   pragma Import (C, LLVMBuildLShr, "LLVMBuildLShr");

   function LLVMBuildAShr
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:869
   pragma Import (C, LLVMBuildAShr, "LLVMBuildAShr");

   function LLVMBuildAnd
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:871
   pragma Import (C, LLVMBuildAnd, "LLVMBuildAnd");

   function LLVMBuildOr
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:873
   pragma Import (C, LLVMBuildOr, "LLVMBuildOr");

   function LLVMBuildXor
     (B    : LLVMBuilderRef;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:875
   pragma Import (C, LLVMBuildXor, "LLVMBuildXor");

   function LLVMBuildBinOp
     (B    : LLVMBuilderRef;
      Op   : LLVMOpcode;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:877
   pragma Import (C, LLVMBuildBinOp, "LLVMBuildBinOp");

   function LLVMBuildNeg
     (B    : LLVMBuilderRef;
      Val  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:880
   pragma Import (C, LLVMBuildNeg, "LLVMBuildNeg");

   function LLVMBuildNSWNeg
     (B    : LLVMBuilderRef;
      Val  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:881
   pragma Import (C, LLVMBuildNSWNeg, "LLVMBuildNSWNeg");

   function LLVMBuildNUWNeg
     (B    : LLVMBuilderRef;
      Val  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:883
   pragma Import (C, LLVMBuildNUWNeg, "LLVMBuildNUWNeg");

   function LLVMBuildFNeg
     (B    : LLVMBuilderRef;
      Val  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:885
   pragma Import (C, LLVMBuildFNeg, "LLVMBuildFNeg");

   function LLVMBuildNot
     (B    : LLVMBuilderRef;
      Val  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:886
   pragma Import (C, LLVMBuildNot, "LLVMBuildNot");

  -- Memory
   function LLVMBuildMalloc
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMTypeRef;
      arg3 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:889
   pragma Import (C, LLVMBuildMalloc, "LLVMBuildMalloc");

   function LLVMBuildArrayMalloc
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMTypeRef;
      arg3 : LLVMValueRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:890
   pragma Import (C, LLVMBuildArrayMalloc, "LLVMBuildArrayMalloc");

   function LLVMBuildAlloca
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMTypeRef;
      arg3 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:892
   pragma Import (C, LLVMBuildAlloca, "LLVMBuildAlloca");

   function LLVMBuildArrayAlloca
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMTypeRef;
      arg3 : LLVMValueRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:893
   pragma Import (C, LLVMBuildArrayAlloca, "LLVMBuildArrayAlloca");

   function LLVMBuildFree (arg1 : LLVMBuilderRef; arg2 : LLVMValueRef) return LLVMValueRef;  -- Core.h:895
   pragma Import (C, LLVMBuildFree, "LLVMBuildFree");

   function LLVMBuildLoad
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:896
   pragma Import (C, LLVMBuildLoad, "LLVMBuildLoad");

   function LLVMBuildStore
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMValueRef) return LLVMValueRef;  -- Core.h:898
   pragma Import (C, LLVMBuildStore, "LLVMBuildStore");

   function LLVMBuildGEP
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : System.Address;
      arg4 : unsigned;
      arg5 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:899
   pragma Import (C, LLVMBuildGEP, "LLVMBuildGEP");

   function LLVMBuildInBoundsGEP
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : System.Address;
      arg4 : unsigned;
      arg5 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:902
   pragma Import (C, LLVMBuildInBoundsGEP, "LLVMBuildInBoundsGEP");

   function LLVMBuildStructGEP
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : unsigned;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:905
   pragma Import (C, LLVMBuildStructGEP, "LLVMBuildStructGEP");

   function LLVMBuildGlobalString
     (arg1 : LLVMBuilderRef;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:907
   pragma Import (C, LLVMBuildGlobalString, "LLVMBuildGlobalString");

   function LLVMBuildGlobalStringPtr
     (arg1 : LLVMBuilderRef;
      arg2 : Interfaces.C.Strings.chars_ptr;
      arg3 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:909
   pragma Import (C, LLVMBuildGlobalStringPtr, "LLVMBuildGlobalStringPtr");

  -- Casts
   function LLVMBuildTrunc
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:913
   pragma Import (C, LLVMBuildTrunc, "LLVMBuildTrunc");

   function LLVMBuildZExt
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:915
   pragma Import (C, LLVMBuildZExt, "LLVMBuildZExt");

   function LLVMBuildSExt
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:917
   pragma Import (C, LLVMBuildSExt, "LLVMBuildSExt");

   function LLVMBuildFPToUI
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:919
   pragma Import (C, LLVMBuildFPToUI, "LLVMBuildFPToUI");

   function LLVMBuildFPToSI
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:921
   pragma Import (C, LLVMBuildFPToSI, "LLVMBuildFPToSI");

   function LLVMBuildUIToFP
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:923
   pragma Import (C, LLVMBuildUIToFP, "LLVMBuildUIToFP");

   function LLVMBuildSIToFP
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:925
   pragma Import (C, LLVMBuildSIToFP, "LLVMBuildSIToFP");

   function LLVMBuildFPTrunc
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:927
   pragma Import (C, LLVMBuildFPTrunc, "LLVMBuildFPTrunc");

   function LLVMBuildFPExt
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:929
   pragma Import (C, LLVMBuildFPExt, "LLVMBuildFPExt");

   function LLVMBuildPtrToInt
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:931
   pragma Import (C, LLVMBuildPtrToInt, "LLVMBuildPtrToInt");

   function LLVMBuildIntToPtr
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:933
   pragma Import (C, LLVMBuildIntToPtr, "LLVMBuildIntToPtr");

   function LLVMBuildBitCast
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:935
   pragma Import (C, LLVMBuildBitCast, "LLVMBuildBitCast");

   function LLVMBuildZExtOrBitCast
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:937
   pragma Import (C, LLVMBuildZExtOrBitCast, "LLVMBuildZExtOrBitCast");

   function LLVMBuildSExtOrBitCast
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:939
   pragma Import (C, LLVMBuildSExtOrBitCast, "LLVMBuildSExtOrBitCast");

   function LLVMBuildTruncOrBitCast
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:941
   pragma Import (C, LLVMBuildTruncOrBitCast, "LLVMBuildTruncOrBitCast");

   function LLVMBuildCast
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMOpcode;
      arg3 : LLVMValueRef;
      arg4 : LLVMTypeRef;
      arg5 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:943
   pragma Import (C, LLVMBuildCast, "LLVMBuildCast");

   function LLVMBuildPointerCast
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:945
   pragma Import (C, LLVMBuildPointerCast, "LLVMBuildPointerCast");

  --Signed cast!
   function LLVMBuildIntCast
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:947
   pragma Import (C, LLVMBuildIntCast, "LLVMBuildIntCast");

   function LLVMBuildFPCast
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:949
   pragma Import (C, LLVMBuildFPCast, "LLVMBuildFPCast");

  -- Comparisons
   function LLVMBuildICmp
     (B    : LLVMBuilderRef;
      Op   : LLVMIntPredicate;
      LHS  : LLVMValueRef;
      RHS  : LLVMValueRef;
      Name : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:953
   pragma Import (C, LLVMBuildICmp, "LLVMBuildICmp");

   function LLVMBuildFCmp
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMRealPredicate;
      arg3 : LLVMValueRef;
      arg4 : LLVMValueRef;
      arg5 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:956
   pragma Import (C, LLVMBuildFCmp, "LLVMBuildFCmp");

  -- Miscellaneous instructions
   function LLVMBuildPhi
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMTypeRef;
      arg3 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:961
   pragma Import (C, LLVMBuildPhi, "LLVMBuildPhi");

   function LLVMBuildCall
     (B       : LLVMBuilderRef;
      Fn      : LLVMValueRef;
      Args    : System.Address;
      NumArgs : unsigned;
      Name    : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:962
   pragma Import (C, LLVMBuildCall, "LLVMBuildCall");

   function LLVMBuildSelect
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMValueRef;
      arg4 : LLVMValueRef;
      arg5 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:965
   pragma Import (C, LLVMBuildSelect, "LLVMBuildSelect");

   function LLVMBuildVAArg
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMTypeRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:968
   pragma Import (C, LLVMBuildVAArg, "LLVMBuildVAArg");

   function LLVMBuildExtractElement
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMValueRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:970
   pragma Import (C, LLVMBuildExtractElement, "LLVMBuildExtractElement");

   function LLVMBuildInsertElement
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMValueRef;
      arg4 : LLVMValueRef;
      arg5 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:972
   pragma Import (C, LLVMBuildInsertElement, "LLVMBuildInsertElement");

   function LLVMBuildShuffleVector
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMValueRef;
      arg4 : LLVMValueRef;
      arg5 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:975
   pragma Import (C, LLVMBuildShuffleVector, "LLVMBuildShuffleVector");

   function LLVMBuildExtractValue
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : unsigned;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:978
   pragma Import (C, LLVMBuildExtractValue, "LLVMBuildExtractValue");

   function LLVMBuildInsertValue
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMValueRef;
      arg4 : unsigned;
      arg5 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:980
   pragma Import (C, LLVMBuildInsertValue, "LLVMBuildInsertValue");

   function LLVMBuildIsNull
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:984
   pragma Import (C, LLVMBuildIsNull, "LLVMBuildIsNull");

   function LLVMBuildIsNotNull
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:986
   pragma Import (C, LLVMBuildIsNotNull, "LLVMBuildIsNotNull");

   function LLVMBuildPtrDiff
     (arg1 : LLVMBuilderRef;
      arg2 : LLVMValueRef;
      arg3 : LLVMValueRef;
      arg4 : Interfaces.C.Strings.chars_ptr) return LLVMValueRef;  -- Core.h:988
   pragma Import (C, LLVMBuildPtrDiff, "LLVMBuildPtrDiff");

  --===-- Module providers --------------------------------------------------===
  -- Changes the type of M so it can be passed to FunctionPassManagers and the
  -- * JIT.  They take ModuleProviders for historical reasons.
  --

   function LLVMCreateModuleProviderForExistingModule (arg1 : LLVMModuleRef) return LLVMModuleProviderRef;  -- Core.h:998
   pragma Import (C, LLVMCreateModuleProviderForExistingModule, "LLVMCreateModuleProviderForExistingModule");

  -- Destroys the module M.
  --

   procedure LLVMDisposeModuleProvider (arg1 : LLVMModuleProviderRef);  -- Core.h:1002
   pragma Import (C, LLVMDisposeModuleProvider, "LLVMDisposeModuleProvider");

  --===-- Memory buffers ----------------------------------------------------===
   function LLVMCreateMemoryBufferWithContentsOfFile
     (arg1 : Interfaces.C.Strings.chars_ptr;
      arg2 : System.Address;
      arg3 : System.Address) return LLVMBool;  -- Core.h:1007
   pragma Import (C, LLVMCreateMemoryBufferWithContentsOfFile, "LLVMCreateMemoryBufferWithContentsOfFile");

   function LLVMCreateMemoryBufferWithSTDIN (arg1 : System.Address; arg2 : System.Address) return LLVMBool;  -- Core.h:1010
   pragma Import (C, LLVMCreateMemoryBufferWithSTDIN, "LLVMCreateMemoryBufferWithSTDIN");

   procedure LLVMDisposeMemoryBuffer (arg1 : LLVMMemoryBufferRef);  -- Core.h:1012
   pragma Import (C, LLVMDisposeMemoryBuffer, "LLVMDisposeMemoryBuffer");

  --===-- Pass Managers -----------------------------------------------------===
  --* Constructs a new whole-module pass pipeline. This type of pipeline is
  --    suitable for link-time optimization and whole-module transformations.
  --    See llvm::PassManager::PassManager.

   function LLVMCreatePassManager return LLVMPassManagerRef;  -- Core.h:1020
   pragma Import (C, LLVMCreatePassManager, "LLVMCreatePassManager");

  --* Constructs a new function-by-function pass pipeline over the module
  --    provider. It does not take ownership of the module provider. This type of
  --    pipeline is suitable for code generation and JIT compilation tasks.
  --    See llvm::FunctionPassManager::FunctionPassManager.

   function LLVMCreateFunctionPassManagerForModule (arg1 : LLVMModuleRef) return LLVMPassManagerRef;  -- Core.h:1026
   pragma Import (C, LLVMCreateFunctionPassManagerForModule, "LLVMCreateFunctionPassManagerForModule");

  --* Deprecated: Use LLVMCreateFunctionPassManagerForModule instead.
   function LLVMCreateFunctionPassManager (arg1 : LLVMModuleProviderRef) return LLVMPassManagerRef;  -- Core.h:1029
   pragma Import (C, LLVMCreateFunctionPassManager, "LLVMCreateFunctionPassManager");

  --* Initializes, executes on the provided module, and finalizes all of the
  --    passes scheduled in the pass manager. Returns 1 if any of the passes
  --    modified the module, 0 otherwise. See llvm::PassManager::run(Module&).

   function LLVMRunPassManager (arg1 : LLVMPassManagerRef; arg2 : LLVMModuleRef) return LLVMBool;  -- Core.h:1034
   pragma Import (C, LLVMRunPassManager, "LLVMRunPassManager");

  --* Initializes all of the function passes scheduled in the function pass
  --    manager. Returns 1 if any of the passes modified the module, 0 otherwise.
  --    See llvm::FunctionPassManager::doInitialization.

   function LLVMInitializeFunctionPassManager (arg1 : LLVMPassManagerRef) return LLVMBool;  -- Core.h:1039
   pragma Import (C, LLVMInitializeFunctionPassManager, "LLVMInitializeFunctionPassManager");

  --* Executes all of the function passes scheduled in the function pass manager
  --    on the provided function. Returns 1 if any of the passes modified the
  --    function, false otherwise.
  --    See llvm::FunctionPassManager::run(Function&).

   function LLVMRunFunctionPassManager (arg1 : LLVMPassManagerRef; arg2 : LLVMValueRef) return LLVMBool;  -- Core.h:1045
   pragma Import (C, LLVMRunFunctionPassManager, "LLVMRunFunctionPassManager");

  --* Finalizes all of the function passes scheduled in in the function pass
  --    manager. Returns 1 if any of the passes modified the module, 0 otherwise.
  --    See llvm::FunctionPassManager::doFinalization.

   function LLVMFinalizeFunctionPassManager (arg1 : LLVMPassManagerRef) return LLVMBool;  -- Core.h:1050
   pragma Import (C, LLVMFinalizeFunctionPassManager, "LLVMFinalizeFunctionPassManager");

  --* Frees the memory of a pass pipeline. For function pipelines, does not free
  --    the module provider.
  --    See llvm::PassManagerBase::~PassManagerBase.

   procedure LLVMDisposePassManager (arg1 : LLVMPassManagerRef);  -- Core.h:1055
   pragma Import (C, LLVMDisposePassManager, "LLVMDisposePassManager");

  -- LLVMModuleProviderRef exists for historical reasons, but now just holds a
  --   * Module.
  --

  -- Specialized opaque context conversions.
  --

  -- Specialized opaque type conversions.
  --

  -- Specialized opaque value conversions.
  --

end Core_h;
