#ifdef BUILD_DAC_LIB
; DAC:	Library routines to manage DAC modules.  Both external DAC
;	(MCP4901) and internal DAC for PIC16F1503 are included.
; Author:	Mike Brady
; Company:	Java Point Pty Ltd
;
;  Assembled with pic-as (v2.32) under MPLAB X IDE (v6.15) 25 Feb 2024
;
;				      PIC16F1503
;			       +----------:_:----------+
;		   +3.3V <>  1 : VDD               VSS : 14 <> GND
;                        <>  2 : RA5               RA0 : 13 <> DAC output
;			 <>  3 : RA4               RA1 : 12 <> 
;			 <>  4 : RA3/MCLR          RA2 : 11 <> DAC output     
;	                 <>  5 : RC5               RC0 : 10 <> 
;                        <>  6 : RC4               RC1 : 9  <> 
;                        <>  7 : RC3               RC2 : 8  <> Red LED
;			       +-----------------------:
;                                       DIP-14

PROCESSOR   16F1503
PAGEWIDTH   132
RADIX       DEC

#include <xc.inc>
#include "DAC.inc"
#include "macros.inc"

PSECT   DACMain,global,class=CODE,delta=2

DAC_init:
    banksel	DACCON0
    movlw       10110000B	   
	       ;1-------	DACEN - enable bit
	       ;-x------	unimplemented
	       ;--1-----	DACOE1 - output on RA0 - use both to provide sufficient current
	       ;---1----	DACOE2 - output on RA2
	       ;----x---	unimplemented
	       ;-----0--	DACPSS - use VDD
	       ;------xx	unimplemented
    movwf       DACCON0
    return
    
DAC_pin_setup:
    ; DAC output is set to RA0 and RA2
     banksel	ANSELA
    movlw	11111010B	; unset ANSA2 and ANSA0 to be digital
    andwf	ANSELA,f
    
    banksel	TRISA
    ; SCK   - RC0 - output
    ; SDI   - RC1 - input
    ; SS    - RC3 - output
    movlw	11110110B	; unset TRISA2 and TRISA0 to be output
    andwf	TRISA,f
    
    return

    ; 8 bit value to write is in WREG on entry
DAC_write:
    banksel	DACCON1
    movwf	DACCON1
    return
    
DAC_disable:
    banksel	DACCON0
    bcf		DACCON0, DACCON0_DACEN_POSN
    return
    
DAC_enable:
    banksel	DACCON0
    bsf		DACCON0, DACCON0_DACEN_POSN
    return
    
MCP4901_init:
    clrf	INTCON		; Disable all interrupts
    banksel	TRISC
    
    ; set alternative SDO pin
    banksel	APFCON
    bsf		APFCON, APFCON_SDOSEL_POSN  ; SDO function is on RA4
    
    ; complete the rest of SPI setup but don't enable just yet
    banksel	SSP1STAT
    movlw       00000000B
	       ;0-------	SMP input data sampled at end of data o/p time
	       ;-0------	CKE Transmit occurs on transition from Idle to active clock state (for DAC requirement)
	       ;-1------	CKE xmit occurs on transition from active to idle clock
	       ;--xxxxxx	I2C only
    movwf       SSP1STAT
    
    ; set SDO function to alternative RA4 pin
    banksel	APFCON
    bsf		APFCON, APFCON_SDOSEL_POSN  ; SDO function is on RA4
    
    ; set the SPI mode and clock speed
    banksel     SSP1CON1
    movlw       00110010B
	       ;0-------	WCOL write collision bit - no collision
	       ;-0------	SSPOV receive overflow indicator bit - no OF
	       ;--1-----	SPEN enable synchronous serial port
	       ;---1----	CKP clock polarity, idle state for clock is high
	       ;----0010	SPI Master mode, Fosc/64
    movwf       SSP1CON1
    
    ; set SS high
    banksel	LATC
    bsf		LATC, LATC_LATC3_POSN
    
    call	stabilisationWait
    
    return
    
MCP4901_pin_setup:
    ; SDO   - RA4 - output
    banksel	ANSELA
    bcf		ANSELA, ANSELA_ANSA4_POSN   ; digital

    banksel	TRISA	
    bcf		TRISA, TRISA_TRISA4_POSN
    
    ; SCK   - RC0 - output
    ; SDI   - RC1 - input
    ; SS    - RC3 - output
    banksel	ANSELC
    movlw	11110110B	; unset ANSC3, ANSC1 and ANSC0 as digital
    andwf	ANSELC,f
    
    banksel	TRISC
    ; SCK   - RC0 - output
    ; SDI   - RC1 - input
    ; SS    - RC3 - output
    movlw	11110110B	; set ANSC1, unset ANSC3 and ANSC0
    andwf	TRISC,f
    
    return

    ; pull Vout to low
MCP4901_shutdown:
    ; now set the MSBs of the buffer value and store result in WReg
    movlw	01100000B
	       ;0-------	Write to DAC register
	       ;-1------	BUF - buffered
	       ;--1-----	GA - Output gain selection is 1x
	       ;---0----	SHDN - Output shutdown control, shutdown the device
	       ;----0000	D7-D4 of requested value
   
    ; pull SS low
    banksel	LATC
    bcf	LATC, LATC_LATC3_POSN
    
    ; write the value to SPI transmit buffer
    banksel	SSP1BUF
    movwf	SSP1BUF
    
    ; wait till BF flag goes high
    banksel	SSP1STAT
    btfss	SSP1STAT, SSP1STAT_BF_POSN
    goto	$-1
    
    ; clear the BF flag by reading SSP1BUF
    banksel	SSP1BUF
    movf	SSP1BUF, w

    clrf	SSP1BUF
    
    ; wait till BF flag goes high
    banksel	SSP1STAT
    btfss	SSP1STAT, SSP1STAT_BF_POSN
    goto	$-1

    ; set SS high
    banksel	LATC
    bsf	LATC, LATC_LATC3_POSN
    
    return
    
    ; 8 bit value to write is in WREG on entry
    ; NB: If flash program memory not being read, PMADRL and PMADRH might be 
    ; usable as temporary working registers.
    ; PMDATL and PMDATH might also be ok?
MCP4901_write_default:
    ; make a copy of the WReg value (the requested voltage level)
    movwf	WRegCopy
    movwf	shiftBuffer	; copy value to shift buffer
    
    ; data written in 2 calls

    ; ==========  prepare the first word
    ; shift buffer value 4 bits to the right
    movlw	4	    ; use WReg as counter
rightShift_def:
    lsrf	shiftBuffer, f
    decfsz	WREG,w
    goto	rightShift_def
    
    ; now set the MSBs of the buffer value and store result in WReg
    movlw	01110000B
	       ;0-------	Write to DAC register
	       ;-1------	BUF - buffered
	       ;--1-----	GA - Output gain selection is 1x
	       ;---1----	SHDN - Output shutdown control, active mode operation
	       ;----0000	D7-D4 of requested value
    iorwf	shiftBuffer,w
    
    ; pull SS low
    banksel	LATC
    bcf	LATC, LATC_LATC3_POSN
    
    ; write the value to SPI transmit buffer
    banksel	SSP1BUF
    movwf	SSP1BUF
    
    ; wait till BF flag goes high
    banksel	SSP1STAT
    btfss	SSP1STAT, SSP1STAT_BF_POSN
    goto	$-1
    
readTheBuf_def:
    ; clear the BF flag by reading SSP1BUF
    banksel	SSP1BUF
    movf	SSP1BUF, w

    ; copy the voltage level into working buffer
    movf	WRegCopy, w
    movwf	shiftBuffer
    ; ==========  prepare the second word
    ; shift the buffer value 4 to the left
    movlw	3
leftShift_def:
    lslf	shiftBuffer, f
    addlw	-1
    skipz
    goto	leftShift_def
    lslf	shiftBuffer,w	; final left shift with result in WReg
    movwf	SSP1BUF
    
    ; wait till BF flag goes high
    banksel	SSP1STAT
    btfss	SSP1STAT, SSP1STAT_BF_POSN
    goto	$-1

    ; set SS high
    banksel	LATC
    bsf	LATC, LATC_LATC3_POSN
    
    return
    
    ; function using alt registers
MCP4901_write_alt:
    ; make a copy of the WReg value (the requested voltage level)
    banksel	SHIFTBUFFER
    movwf	SHIFTBUFFER
    movwf	WREG_COPY
    
    ; data written in 2 calls

    ; ==========  prepare the first word
    ; shift buffer value 4 bits to the right
    movlw	4	    ; use WReg as counter
rightShift_alt:
    lsrf	SHIFTBUFFER, f
    decfsz	WREG,w
    goto	rightShift_alt
    
    ; now set the MSBs of the buffer value and store result in WReg
    movlw	01110000B
	       ;0-------	Write to DAC register
	       ;-1------	BUF - buffered
	       ;--1-----	GA - Output gain selection is 1x
	       ;---1----	SHDN - Output shutdown control, active mode operation
	       ;----0000	D7-D4 of requested value
    iorwf	SHIFTBUFFER,w
    
    ; pull SS low
    banksel	LATC
    bcf	LATC, LATC_LATC3_POSN
    
    ; write the value to SPI transmit buffer
    banksel	SSP1BUF
    movwf	SSP1BUF
    
    ; wait till BF flag goes high
    banksel	SSP1STAT
    btfss	SSP1STAT, SSP1STAT_BF_POSN
    goto	$-1
    
readTheBuf_alt:
    ; clear the BF flag by reading SSP1BUF
    banksel	SSP1BUF
    movf	SSP1BUF, w

    ; copy the voltage level into working buffer
    banksel	SHIFTBUFFER
    movf	WREG_COPY,w
    movwf	SHIFTBUFFER
    ; ==========  prepare the second word
    ; shift the buffer value 4 to the left
    movlw	3
leftShift_alt:
    lslf	SHIFTBUFFER, f
    addlw	-1
    skipz
    goto	leftShift_alt
    lslf	SHIFTBUFFER, w
    banksel	SSP1BUF
    movwf	SSP1BUF
    
    ; wait till BF flag goes high
    banksel	SSP1STAT
    btfss	SSP1STAT, SSP1STAT_BF_POSN
    goto	$-1

    ; set SS high
    banksel	LATC
    bsf	LATC, LATC_LATC3_POSN
    
    return
    
stabilisationWait:
    call	delay1s
    return
 
delay1s:
    movlw	0x23
    movwf	d1
    movlw	0xB9
    movwf	d2
    movlw	0x09
    movwf	d3
delay1s_0:
    decfsz	d1, f
    goto	$+2
    decfsz	d2, f
    goto	$+2
    decfsz	d3, f
    goto	delay1s_0
    goto	$+1
    return    
    
END 
#endif
