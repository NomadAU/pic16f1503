#ifndef MACROS_INCLUDED
#define MACROS_INCLUDED
skipnc  MACRO
  btfsc   STATUS,STATUS_C_POSITION
ENDM

skipc  MACRO
  btfss   STATUS,STATUS_C_POSITION
ENDM

skipnz  MACRO
  btfsc   STATUS,STATUS_Z_POSITION
ENDM

skipz  MACRO
  btfss   STATUS,STATUS_Z_POSITION
ENDM

#endif
