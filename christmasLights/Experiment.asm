#if EXPERIMENT
;
; File:     main.S
; Target:   PIC16f84A
; Author:   dan1138
; Date:     2020-08-20
; Compiler: pic-as(v2.20)
; IDE:      MPLABX v5.40
;
; Description:
;
;  Example project for the PIC16F1503 controller using the pic-as(v2.45) tool chain.
;  Assembled with pic-as (v2.32) under MPLAB X IDE (v6.15) 28 Jan 2024
;
; Add this line in the project properties box, pic-as Global Options -> Additional options: 
;   -Wa,-a -Wl,-pPOR_Vec=0h,-pISR_Vec=4h,-pData_Vec=70h
;
;                            PIC16F1503
;                     +----------:_:----------+
;         +3.3V <>  1 : VDD               VSS : 14 <> GND
;               <>  2 : RA5               RA0 : 13 <> 
;               <>  3 : RA4               RA1 : 12 <> 
;               <>  4 : RA3/MCLR          RA2 : 11 <>      
;               <>  5 : RC5               RC0 : 10 <> 
;               <>  6 : RC4               RC1 : 9  <> 
;               <>  7 : RC3               RC2 : 8  <> 
;                     +-----------------------:
;                                DIP-14
	

PAGEWIDTH   132
RADIX       DEC

#include <xc.inc>
#include "macros.inc"

; PIC16F1503 Configuration Bit Settings
config FOSC = INTOSC    ; Oscillator Selection bits (HS oscillator)
config WDTE = OFF       ; Watchdog Timer (WDT disabled)
config PWRTE = OFF      ; Power-up Timer Enable bit (Power-up Timer is disabled)
config CP = OFF         ; Code Protection bit (Code protection disabled)
config MCLRE = ON
config BOREN = ON
config CLKOUTEN = OFF
config WRT = OFF
config STVREN = OFF
config LVP = OFF
config LPBOR = OFF
config BORV = LO

; ============== macros ===============
  
; ============== File registers for variables (max 16 words) =================
PSECT Data_Vec,global,class=myData,space=1,delta=1,noexec
    dummyVar:      DS  1
    
END_Data_Vec:   
    
; ============== Power-on entry point =================
PSECT POR_Vec,class=CODE,delta=2,reloc=2
POR_Vec:
    nop
    goto    main

; ============== Interrupt vector and handler =================
PSECT ISR_Vec,global,class=CODE,delta=2
    GLOBAL  ISR_Vec

ISR_Vec:
    retfie
    
END_ISR_Vec:
        
main:
loop:
    skipnc    
    nop
    goto	loop
	
END POR_Vec
#endif