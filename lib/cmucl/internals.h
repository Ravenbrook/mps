/*
 * Machine generated header file.  Do not edit.
 */

#ifndef _INTERNALS_H_
#define _INTERNALS_H_

#define type_EvenFixnum 0
#define type_FunctionPointer 1
#define type_OtherImmediate0 2
#define type_ListPointer 3
#define type_OddFixnum 4
#define type_InstancePointer 5
#define type_OtherImmediate1 6
#define type_OtherPointer 7
#define type_Bignum 10
#define type_Ratio 14
#define type_SingleFloat 18
#define type_DoubleFloat 22
#define type_DoubleDoubleFloat 26
#define type_Complex 30
#define type_ComplexSingleFloat 34
#define type_ComplexDoubleFloat 38
#define type_ComplexDoubleDoubleFloat 42
#define type_SimpleArray 46
#define type_SimpleString 50
#define type_SimpleBitVector 54
#define type_SimpleVector 58
#define type_SimpleArrayUnsignedByte2 62
#define type_SimpleArrayUnsignedByte4 66
#define type_SimpleArrayUnsignedByte8 70
#define type_SimpleArrayUnsignedByte16 74
#define type_SimpleArrayUnsignedByte32 78
#define type_SimpleArraySignedByte8 82
#define type_SimpleArraySignedByte16 86
#define type_SimpleArraySignedByte30 90
#define type_SimpleArraySignedByte32 94
#define type_SimpleArraySingleFloat 98
#define type_SimpleArrayDoubleFloat 102
#define type_SimpleArrayDoubleDoubleFloat 106
#define type_SimpleArrayComplexSingleFloat 110
#define type_SimpleArrayComplexDoubleFloat 114
#define type_SimpleArrayComplexDoubleDoubleFloat 118
#define type_ComplexString 122
#define type_ComplexBitVector 126
#define type_ComplexVector 130
#define type_ComplexArray 134
#define type_CodeHeader 138
#define type_FunctionHeader 142
#define type_ClosureHeader 146
#define type_FuncallableInstanceHeader 150
#define type_ByteCodeFunction 154
#define type_ByteCodeClosure 158
#define type_ClosureFunctionHeader 162
#define type_ReturnPcHeader 166
#define type_ValueCellHeader 170
#define type_SymbolHeader 174
#define type_BaseChar 178
#define type_Sap 182
#define type_UnboundMarker 186
#define type_WeakPointer 190
#define type_InstanceHeader 194
#define type_Fdefn 198
#define type_ScavengerHook 202

#define trap_Halt 8
#define trap_PendingInterrupt 9
#define trap_Error 10
#define trap_Cerror 11
#define trap_Breakpoint 12
#define trap_FunctionEndBreakpoint 13
#define trap_SingleStepBreakpoint 14
#define trap_DynamicSpaceOverflowWarning 15
#define trap_ObjectNotList 16
#define trap_DynamicSpaceOverflowError 16
#define trap_ObjectNotInstance 17

#define subtype_VectorNormal 0
#define subtype_VectorValidHashing 2
#define subtype_VectorMustRehash 3

#define tracetab_Normal 0
#define tracetab_CallSite 1
#define tracetab_FunctionPrologue 2
#define tracetab_FunctionEpilogue 3

#define sc_Constant 0
#define sc_FpConstant 1
#define sc_Immediate 2
#define sc_ControlStack 3
#define sc_SignedStack 4
#define sc_UnsignedStack 5
#define sc_BaseCharStack 6
#define sc_SapStack 7
#define sc_SingleStack 8
#define sc_DoubleStack 9
#define sc_DoubleDoubleStack 10
#define sc_ComplexSingleStack 11
#define sc_ComplexDoubleStack 12
#define sc_ComplexDoubleDoubleStack 13
#define sc_IgnoreMe 14
#define sc_AnyReg 15
#define sc_DescriptorReg 16
#define sc_BaseCharReg 17
#define sc_SapReg 18
#define sc_SignedReg 19
#define sc_UnsignedReg 20
#define sc_WordReg 21
#define sc_ByteReg 22
#define sc_SingleReg 23
#define sc_DoubleReg 24
#define sc_DoubleDoubleReg 25
#define sc_ComplexSingleReg 26
#define sc_ComplexDoubleReg 27
#define sc_ComplexDoubleDoubleReg 28
#define sc_CatchBlock 29

#define LinkageEntrySize 8
#define LinkageSpaceStart 0xB0000000U

#define lowtag_Bits 3  /* Number of bits at the low end of a pointer used for type information. */
#define lowtag_Mask 7  /* Mask to extract the low tag bits from a pointer. */
#define lowtag_Limit 8  /* Exclusive upper bound on the value of the low tag bits from a
  pointer. */

#define type_Bits 8  /* Number of bits used in the header word of a data block for typeing. */
#define type_Mask 255  /* Mask to extract the type from a header word. */

#define SpaceStart_TargetReadOnly 0x10000000U
#define SpaceStart_TargetStatic 0x28F00000U
#define SpaceStart_TargetDynamic 0x48000000U
#define SpaceStart_TargetForeignLinkage 0xB0000000U

#define UNKNOWN_ERROR 0
#define OBJECT_NOT_FUNCTION_ERROR 1
#define OBJECT_NOT_LIST_ERROR 2
#define OBJECT_NOT_BIGNUM_ERROR 3
#define OBJECT_NOT_RATIO_ERROR 4
#define OBJECT_NOT_SINGLE_FLOAT_ERROR 5
#define OBJECT_NOT_DOUBLE_FLOAT_ERROR 6
#define OBJECT_NOT_DOUBLE_DOUBLE_FLOAT_ERROR 7
#define OBJECT_NOT_SIMPLE_STRING_ERROR 8
#define OBJECT_NOT_SIMPLE_BIT_VECTOR_ERROR 9
#define OBJECT_NOT_SIMPLE_VECTOR_ERROR 10
#define OBJECT_NOT_FIXNUM_ERROR 11
#define OBJECT_NOT_FUNCTION_OR_SYMBOL_ERROR 12
#define OBJECT_NOT_VECTOR_ERROR 13
#define OBJECT_NOT_STRING_ERROR 14
#define OBJECT_NOT_BIT_VECTOR_ERROR 15
#define OBJECT_NOT_ARRAY_ERROR 16
#define OBJECT_NOT_NUMBER_ERROR 17
#define OBJECT_NOT_RATIONAL_ERROR 18
#define OBJECT_NOT_FLOAT_ERROR 19
#define OBJECT_NOT_REAL_ERROR 20
#define OBJECT_NOT_INTEGER_ERROR 21
#define OBJECT_NOT_CONS_ERROR 22
#define OBJECT_NOT_SYMBOL_ERROR 23
#define UNDEFINED_SYMBOL_ERROR 24
#define OBJECT_NOT_COERCABLE_TO_FUNCTION_ERROR 25
#define INVALID_ARGUMENT_COUNT_ERROR 26
#define BOGUS_ARGUMENT_TO_VALUES_LIST_ERROR 27
#define UNBOUND_SYMBOL_ERROR 28
#define OBJECT_NOT_SAP_ERROR 30
#define INVALID_UNWIND_ERROR 31
#define UNSEEN_THROW_TAG_ERROR 32
#define DIVISION_BY_ZERO_ERROR 33
#define OBJECT_NOT_TYPE_ERROR 34
#define ODD_KEYWORD_ARGUMENTS_ERROR 35
#define UNKNOWN_KEYWORD_ARGUMENT_ERROR 36
#define INVALID_ARRAY_INDEX_ERROR 39
#define WRONG_NUMBER_OF_INDICES_ERROR 40
#define OBJECT_NOT_SIMPLE_ARRAY_ERROR 41
#define OBJECT_NOT_SIGNED_BYTE_32_ERROR 42
#define OBJECT_NOT_UNSIGNED_BYTE_32_ERROR 43
#define OBJECT_NOT_SIMPLE_ARRAY_UNSIGNED_BYTE_2_ERROR 44
#define OBJECT_NOT_SIMPLE_ARRAY_UNSIGNED_BYTE_4_ERROR 45
#define OBJECT_NOT_SIMPLE_ARRAY_UNSIGNED_BYTE_8_ERROR 46
#define OBJECT_NOT_SIMPLE_ARRAY_UNSIGNED_BYTE_16_ERROR 47
#define OBJECT_NOT_SIMPLE_ARRAY_UNSIGNED_BYTE_32_ERROR 48
#define OBJECT_NOT_SIMPLE_ARRAY_SIGNED_BYTE_8_ERROR 49
#define OBJECT_NOT_SIMPLE_ARRAY_SIGNED_BYTE_16_ERROR 50
#define OBJECT_NOT_SIMPLE_ARRAY_SIGNED_BYTE_30_ERROR 51
#define OBJECT_NOT_SIMPLE_ARRAY_SIGNED_BYTE_32_ERROR 52
#define OBJECT_NOT_SIMPLE_ARRAY_SINGLE_FLOAT_ERROR 53
#define OBJECT_NOT_SIMPLE_ARRAY_DOUBLE_FLOAT_ERROR 54
#define OBJECT_NOT_SIMPLE_ARRAY_DOUBLE_DOUBLE_FLOAT_ERROR 55
#define OBJECT_NOT_SIMPLE_ARRAY_COMPLEX_SINGLE_FLOAT_ERROR 56
#define OBJECT_NOT_SIMPLE_ARRAY_COMPLEX_DOUBLE_FLOAT_ERROR 57
#define OBJECT_NOT_SIMPLE_ARRAY_COMPLEX_DOUBLE_DOUBLE_FLOAT_ERROR 58
#define OBJECT_NOT_COMPLEX_ERROR 59
#define OBJECT_NOT_COMPLEX_RATIONAL_ERROR 60
#define OBJECT_NOT_COMPLEX_FLOAT_ERROR 61
#define OBJECT_NOT_COMPLEX_SINGLE_FLOAT_ERROR 62
#define OBJECT_NOT_COMPLEX_DOUBLE_FLOAT_ERROR 63
#define OBJECT_NOT_COMPLEX_DOUBLE_DOUBLE_FLOAT_ERROR 64
#define OBJECT_NOT_WEAK_POINTER_ERROR 65
#define OBJECT_NOT_INSTANCE_ERROR 66
#define OBJECT_NOT_BASE_CHAR_ERROR 67
#define NIL_FUNCTION_RETURNED_ERROR 68
#define LAYOUT_INVALID_ERROR 69
#define UNDEFINED_FOREIGN_SYMBOL_ERROR 70

#define ERRORS { \
    "Unknown.  System lossage.", \
    "Object is not of type FUNCTION.", \
    "Object is not of type LIST.", \
    "Object is not of type BIGNUM.", \
    "Object is not of type RATIO.", \
    "Object is not of type SINGLE-FLOAT.", \
    "Object is not of type DOUBLE-FLOAT.", \
    "Object is not of type DOUBLE-DOUBLE-FLOAT.", \
    "Object is not of type SIMPLE-STRING.", \
    "Object is not of type SIMPLE-BIT-VECTOR.", \
    "Object is not of type SIMPLE-VECTOR.", \
    "Object is not of type FIXNUM.", \
    "Object is not of type FUNCTION or SYMBOL.", \
    "Object is not of type VECTOR.", \
    "Object is not of type STRING.", \
    "Object is not of type BIT-VECTOR.", \
    "Object is not of type ARRAY.", \
    "Object is not of type NUMBER.", \
    "Object is not of type RATIONAL.", \
    "Object is not of type FLOAT.", \
    "Object is not of type REAL.", \
    "Object is not of type INTEGER.", \
    "Object is not of type CONS.", \
    "Object is not of type SYMBOL.", \
    "Undefined symbol.", \
    "Object is not coercable to type FUNCTION.", \
    "Invalid argument count.", \
    "Bogus argument to VALUES-LIST.", \
    "Unbound symbol.", \
    "unused", \
    "Object is not a System Area Pointer (SAP).", \
    "Attempt to RETURN-FROM a block that no longer exists.", \
    "Attempt to THROW to a non-existent tag.", \
    "Attempt to divide by zero.", \
    "Object is of the wrong type.", \
    "Odd number of keyword arguments.", \
    "Unknown keyword.", \
    "unused", \
    "unused", \
    "Invalid array index.", \
    "Wrong number of indices.", \
    "Object is not of type SIMPLE-ARRAY.", \
    "Object is not of type (SIGNED-BYTE 32).", \
    "Object is not of type (UNSIGNED-BYTE 32).", \
    "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 2) (*)).", \
    "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 4) (*)).", \
    "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)).", \
    "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 16) (*)).", \
    "Object is not of type (SIMPLE-ARRAY (UNSIGNED-BYTE 32) (*)).", \
    "Object is not of type (SIMPLE-ARRAY (SIGNED-BYTE 8) (*)).", \
    "Object is not of type (SIMPLE-ARRAY (SIGNED-BYTE 16) (*)).", \
    "Object is not of type (SIMPLE-ARRAY FIXNUM (*)).", \
    "Object is not of type (SIMPLE-ARRAY (SIGNED-BYTE 32) (*)).", \
    "Object is not of type (SIMPLE-ARRAY SINGLE-FLOAT (*)).", \
    "Object is not of type (SIMPLE-ARRAY DOUBLE-FLOAT (*)).", \
    "Object is not of type (SIMPLE-ARRAY DOUBLE-DOUBLE-FLOAT (*)).", \
    "Object is not of type (SIMPLE-ARRAY (COMPLEX SINGLE-FLOAT) (*)).", \
    "Object is not of type (SIMPLE-ARRAY (COMPLEX DOUBLE-FLOAT) (*)).", \
    "Object is not of type (SIMPLE-ARRAY (COMPLEX DOUBLE-DOUBLE-FLOAT) (*)).", \
    "Object is not of type COMPLEX.", \
    "Object is not of type (COMPLEX RATIONAL).", \
    "Object is not of type (COMPLEX FLOAT).", \
    "Object is not of type (COMPLEX SINGLE-FLOAT).", \
    "Object is not of type (COMPLEX DOUBLE-FLOAT).", \
    "Object is not of type (COMPLEX DOUBLE-DOUBLE-FLOAT)", \
    "Object is not a WEAK-POINTER.", \
    "Object is not a INSTANCE.", \
    "Object is not of type BASE-CHAR.", \
    "Function with declared result type NIL returned.", \
    "Layout is invalid (instance obsolete.)", \
    "No value for foreign symbol.", \
    NULL \
}

#ifndef LANGUAGE_ASSEMBLY

#define LISPOBJ(x) ((lispobj)x)

struct array {
    lispobj header;
    lispobj fill_pointer;
    lispobj fill_pointer_p;
    lispobj elements;
    lispobj data;
    lispobj displacement;
    lispobj displaced_p;
    lispobj dimensions[1];
};

struct bignum {
    lispobj header;
    long digits[1];
};

struct binding {
    lispobj value;
    lispobj symbol;
};

struct catch_block {
    struct unwind_block * current_uwp;
    lispobj * current_cont;
    lispobj entry_pc;
    lispobj tag;
    struct catch_block * previous_catch;
    lispobj size;
};

struct closure {
    lispobj header;
    lispobj function;
    lispobj info[1];
};

struct code {
    lispobj header;
    lispobj code_size;
    lispobj entry_points;
    lispobj debug_info;
    lispobj trace_table_offset;
    lispobj constants[1];
};

struct complex {
    lispobj header;
    lispobj real;
    lispobj imag;
};

struct complex_double_double_float {
    lispobj header;
    lispobj filler;
    double real_hi;
    double real_lo;
    double imag_hi;
    double imag_lo;
};

struct complex_double_float {
    lispobj header;
    lispobj filler;
    double real;
    double imag;
};

struct complex_single_float {
    lispobj header;
    float real;
    float imag;
};

struct cons {
    lispobj car;
    lispobj cdr;
};

struct double_double_float {
    lispobj header;
    lispobj filler;
    double hi;
    double lo;
};

struct double_float {
    lispobj header;
    lispobj filler;
    double value;
};

struct fdefn {
    lispobj header;
    lispobj name;
    lispobj function;
    char * raw_addr;
};

struct funcallable_instance {
    lispobj header;
    lispobj function;
    lispobj lexenv;
    lispobj layout;
    lispobj info[1];
};

struct function {
    lispobj header;
    lispobj self;
    lispobj next;
    lispobj name;
    lispobj arglist;
    lispobj type;
    unsigned char code[1];
};

struct instance {
    lispobj header;
    lispobj slots[1];
};

struct ratio {
    lispobj header;
    lispobj numerator;
    lispobj denominator;
};

struct return_pc {
    lispobj header;
    unsigned char return_point[1];
};

struct sap {
    lispobj header;
    char * pointer;
};

struct scavenger_hook {
    lispobj header;
    lispobj value;
    lispobj function;
    struct scavenger_hook * next;
};

struct single_float {
    lispobj header;
    float value;
};

struct symbol {
    lispobj header;
    lispobj value;
    lispobj hash;
    lispobj plist;
    lispobj name;
    lispobj package;
};

struct unwind_block {
    struct unwind_block * current_uwp;
    lispobj * current_cont;
    lispobj entry_pc;
};

struct value_cell {
    lispobj header;
    lispobj value;
};

struct vector {
    lispobj header;
    lispobj length;
    unsigned long data[1];
};

struct weak_pointer {
    lispobj header;
    lispobj value;
    lispobj broken;
    lispobj mark_bit;
    struct weak_pointer * next;
};

#else /* LANGUAGE_ASSEMBLY */

#define LISPOBJ(thing) thing

#define ARRAY_FILL_POINTER_OFFSET -3
#define ARRAY_FILL_POINTER_P_OFFSET 1
#define ARRAY_ELEMENTS_OFFSET 5
#define ARRAY_DATA_OFFSET 9
#define ARRAY_DISPLACEMENT_OFFSET 13
#define ARRAY_DISPLACED_P_OFFSET 17
#define ARRAY_DIMENSIONS_OFFSET 21

#define BIGNUM_DIGITS_OFFSET -3

#define CLOSURE_FUNCTION_OFFSET 3
#define CLOSURE_INFO_OFFSET 7

#define CODE_CODE_SIZE_OFFSET -3
#define CODE_ENTRY_POINTS_OFFSET 1
#define CODE_DEBUG_INFO_OFFSET 5
#define CODE_TRACE_TABLE_OFFSET_OFFSET 9
#define CODE_CONSTANTS_OFFSET 13

#define COMPLEX_REAL_OFFSET -3
#define COMPLEX_IMAG_OFFSET 1

#define COMPLEX_DOUBLE_DOUBLE_FLOAT_FILLER_OFFSET -3
#define COMPLEX_DOUBLE_DOUBLE_FLOAT_REAL_HI_OFFSET 1
#define COMPLEX_DOUBLE_DOUBLE_FLOAT_REAL_LO_OFFSET 9
#define COMPLEX_DOUBLE_DOUBLE_FLOAT_IMAG_HI_OFFSET 17
#define COMPLEX_DOUBLE_DOUBLE_FLOAT_IMAG_LO_OFFSET 25

#define COMPLEX_DOUBLE_FLOAT_FILLER_OFFSET -3
#define COMPLEX_DOUBLE_FLOAT_REAL_OFFSET 1
#define COMPLEX_DOUBLE_FLOAT_IMAG_OFFSET 9

#define COMPLEX_SINGLE_FLOAT_REAL_OFFSET -3
#define COMPLEX_SINGLE_FLOAT_IMAG_OFFSET 1

#define CONS_CAR_OFFSET -3
#define CONS_CDR_OFFSET 1

#define DOUBLE_DOUBLE_FLOAT_FILLER_OFFSET -3
#define DOUBLE_DOUBLE_FLOAT_HI_OFFSET 1
#define DOUBLE_DOUBLE_FLOAT_LO_OFFSET 9

#define DOUBLE_FLOAT_FILLER_OFFSET -3
#define DOUBLE_FLOAT_VALUE_OFFSET 1

#define FDEFN_NAME_OFFSET -3
#define FDEFN_FUNCTION_OFFSET 1
#define FDEFN_RAW_ADDR_OFFSET 5

#define FUNCALLABLE_INSTANCE_FUNCTION_OFFSET 3
#define FUNCALLABLE_INSTANCE_LEXENV_OFFSET 7
#define FUNCALLABLE_INSTANCE_LAYOUT_OFFSET 11
#define FUNCALLABLE_INSTANCE_INFO_OFFSET 15

#define FUNCTION_SELF_OFFSET 3
#define FUNCTION_NEXT_OFFSET 7
#define FUNCTION_NAME_OFFSET 11
#define FUNCTION_ARGLIST_OFFSET 15
#define FUNCTION_TYPE_OFFSET 19
#define FUNCTION_CODE_OFFSET 23

#define INSTANCE_SLOTS_OFFSET -1

#define RATIO_NUMERATOR_OFFSET -3
#define RATIO_DENOMINATOR_OFFSET 1

#define RETURN_PC_RETURN_POINT_OFFSET -3

#define SAP_POINTER_OFFSET -3

#define SCAVENGER_HOOK_VALUE_OFFSET -3
#define SCAVENGER_HOOK_FUNCTION_OFFSET 1
#define SCAVENGER_HOOK_NEXT_OFFSET 5

#define SINGLE_FLOAT_VALUE_OFFSET -3

#define SYMBOL_VALUE_OFFSET -3
#define SYMBOL_HASH_OFFSET 1
#define SYMBOL_PLIST_OFFSET 5
#define SYMBOL_NAME_OFFSET 9
#define SYMBOL_PACKAGE_OFFSET 13

#define VALUE_CELL_VALUE_OFFSET -3

#define VECTOR_LENGTH_OFFSET -3
#define VECTOR_DATA_OFFSET 1

#define WEAK_POINTER_VALUE_OFFSET -3
#define WEAK_POINTER_BROKEN_OFFSET 1
#define WEAK_POINTER_MARK_BIT_OFFSET 5
#define WEAK_POINTER_NEXT_OFFSET 9

#endif /* LANGUAGE_ASSEMBLY */

#define NIL LISPOBJ(0x28F0000B)
#define T LISPOBJ(0x28F00027)
#define LISP_ENVIRONMENT_LIST LISPOBJ(0x28F0003F)
#define LISP_COMMAND_LINE_LIST LISPOBJ(0x28F00057)
#define BATCH_MODE LISPOBJ(0x28F0006F)
#define INITIAL_FDEFN_OBJECTS LISPOBJ(0x28F00087)
#define INITIAL_FUNCTION LISPOBJ(0x28F0009F)
#define MAYBE_GC LISPOBJ(0x28F000B7)
#define INTERNAL_ERROR LISPOBJ(0x28F000CF)
#define YELLOW_ZONE_HIT LISPOBJ(0x28F000E7)
#define RED_ZONE_HIT LISPOBJ(0x28F000FF)
#define DYNAMIC_SPACE_OVERFLOW_WARNING_HIT LISPOBJ(0x28F00117)
#define DYNAMIC_SPACE_OVERFLOW_ERROR_HIT LISPOBJ(0x28F0012F)
#define HANDLE_BREAKPOINT LISPOBJ(0x28F00147)
#define FDEFINITION_OBJECT LISPOBJ(0x28F0015F)
#define READ_ONLY_SPACE_FREE_POINTER LISPOBJ(0x28F00177)
#define STATIC_SPACE_FREE_POINTER LISPOBJ(0x28F0018F)
#define INITIAL_DYNAMIC_SPACE_FREE_POINTER LISPOBJ(0x28F001A7)
#define CURRENT_CATCH_BLOCK LISPOBJ(0x28F001BF)
#define CURRENT_UNWIND_PROTECT_BLOCK LISPOBJ(0x28F001D7)
#define EVAL_STACK_TOP LISPOBJ(0x28F001EF)
#define ALIEN_STACK LISPOBJ(0x28F00207)
#define PSEUDO_ATOMIC_ATOMIC LISPOBJ(0x28F0021F)
#define PSEUDO_ATOMIC_INTERRUPTED LISPOBJ(0x28F00237)
#define INTERRUPTS_ENABLED LISPOBJ(0x28F0024F)
#define INTERRUPT_PENDING LISPOBJ(0x28F00267)
#define FREE_INTERRUPT_CONTEXT_INDEX LISPOBJ(0x28F0027F)
#define ALLOCATION_POINTER LISPOBJ(0x28F00297)
#define BINDING_STACK_POINTER LISPOBJ(0x28F002AF)
#define INTERNAL_GC_TRIGGER LISPOBJ(0x28F002C7)
#define FP_CONSTANT_0D0 LISPOBJ(0x28F002DF)
#define FP_CONSTANT_1D0 LISPOBJ(0x28F002F7)
#define FP_CONSTANT_0S0 LISPOBJ(0x28F0030F)
#define FP_CONSTANT_1S0 LISPOBJ(0x28F00327)
#define FP_CONSTANT_0L0 LISPOBJ(0x28F0033F)
#define FP_CONSTANT_1L0 LISPOBJ(0x28F00357)
#define FP_CONSTANT_PI LISPOBJ(0x28F0036F)
#define FP_CONSTANT_L2T LISPOBJ(0x28F00387)
#define FP_CONSTANT_L2E LISPOBJ(0x28F0039F)
#define FP_CONSTANT_LG2 LISPOBJ(0x28F003B7)
#define FP_CONSTANT_LN2 LISPOBJ(0x28F003CF)
#define SCAVENGE_READ_ONLY_SPACE LISPOBJ(0x28F003E7)
#define CONTROL_STACKS LISPOBJ(0x28F003FF)
#define SLOT_UNBOUND LISPOBJ(0x28F00417)
#define CMUCL_LIB LISPOBJ(0x28F0042F)
#define CMUCL_CORE_PATH LISPOBJ(0x28F00447)
#define KEY LISPOBJ(0x28F0045F)
#define VALUE LISPOBJ(0x28F00477)
#define KEY_AND_VALUE LISPOBJ(0x28F0048F)
#define KEY_OR_VALUE LISPOBJ(0x28F004A7)
#define SPARE_9 LISPOBJ(0x28F004BF)
#define SPARE_8 LISPOBJ(0x28F004D7)
#define SPARE_7 LISPOBJ(0x28F004EF)
#define SPARE_6 LISPOBJ(0x28F00507)
#define SPARE_5 LISPOBJ(0x28F0051F)
#define SPARE_4 LISPOBJ(0x28F00537)
#define SPARE_3 LISPOBJ(0x28F0054F)
#define SPARE_2 LISPOBJ(0x28F00567)
#define SPARE_1 LISPOBJ(0x28F0057F)
#define X86_CGC_ACTIVE_P LISPOBJ(0x28F00597)
#define LINKAGE_TABLE_DATA LISPOBJ(0x28F005AF)
#define GLOBAL_TABLE LISPOBJ(0x28F005C7)
#define CURRENT_REGION_FREE_POINTER LISPOBJ(0x28F005DF)
#define CURRENT_REGION_END_ADDR LISPOBJ(0x28F005F7)
#define STATIC_BLUE_BAG LISPOBJ(0x28F0060F)

/* Runtime features when built. */

#define FEATURE_MP 1
#define FEATURE_LINKAGE_TABLE 1
#define FEATURE_GENCGC 1
#define FEATURE_PENTIUM 1
#define FEATURE_I486 1
#define FEATURE_HASH_NEW 1
#define FEATURE_HEAP_OVERFLOW_CHECK 1
#define FEATURE_STACK_CHECKING 1

#endif
