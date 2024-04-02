#ifdef BUILD_I2C_LIB
; Library containing I2C routines for PIC16F1503
; Author:	Mike Brady
; Company:	Java Point Pty Ltd
;

PROCESSOR   16F1503
PAGEWIDTH   132
RADIX       DEC

#include <xc.inc>
#include "macros.inc"
#include "I2C.inc"
    
PSECT   I2CMain,global,class=CODE,delta=2

I2C_MasterInit:
    ;Configure MSSP module for Master Mode
    banksel	SSP1CON1
    movlw	00101000B   ; Enables MSSP and uses appropriate
			    ; PORTC pins for I2C mode (SSPEN set) AND
			    ; Enables I2C Master Mode (SSPMx bits)
    movwf	SSP1CON1	    ; This is loaded into SSPCON
    ; Configure Input Levels and slew rate as I2C Standard Levels
    banksel	SSP1STAT
    movlw	10000000B   ; Slew Rate control (SMP) set for 100kHz
    movwf	SSP1STAT	    ; mode and input levels are I2C spec,
    ; loaded in SSP1STAT

    ; Configure Baud Rate
    banksel	SSP1ADD
    movlw	FOSC_KHZ / (4 * BAUD) - 1	; Calculates SSPADD Setting for
    movwf	SSP1ADD			; desired Baud rate and sets up SSPADD
    
    clrf	I2C_control
    call	stabilisationWait
    
    return
    
I2C_SlaveInit:	; slave address in WReg on entry
    movwf	slaveAddress
    banksel	SSP1CON1
    clrf	SSP1CON1
    clrf	SSP1CON2
    clrf	SSP1CON3
    clrf	SSP1STAT
    
    bsf		SSP1STAT, SSP1STAT_SMP_POSN ; Disable slew control for Standard mode
    movlw	00000110B   ; Set MSSP Operating Mode (7-bit Client)
    iorwf	SSP1CON1,f  
    bsf		SSP1CON2, SSP1CON2_SEN_POSN ; Enable clock stretching
    bsf		SSP1CON3, SSP1CON3_SBCDE_POSN ; Enable bus collision interrupts
    
    movf	slaveAddress,w
    movwf	SSP1ADD
    lslf	SSP1ADD,f   ; shift value left by one
    
    banksel	PIR2
    bcf		PIR2, PIR2_BCL1IF_POSN	; Clear Bus Collision interrupt flag
    bcf		PIR1, PIR1_SSP1IF_POSN	; Clear the SSP interrupt flag
    banksel	PIE2
    bsf		PIE2, PIE2_BCL1IE_POSN	; Enable bus collision interrupt
    bsf		PIE1, PIE1_SSP1IE_POSN	; Enable MSSP interrupt
    banksel	SSP1CON1
    bsf		SSP1CON1, SSP1CON1_SSPEN_POSN
    bsf		INTCON, INTCON_PEIE_POSN
    bsf		INTCON, INTCON_GIE_POSN
    
    clrf	I2C_control
    call	stabilisationWait
    
    return
        
I2C_InitI2CPins:
    ; RC0=SCL, RC1=SDA
    banksel	ANSELC	; I2C pins are digital
    bcf		ANSELC,ANSELC_ANSC0_POSN
    bcf		ANSELC,ANSELC_ANSC1_POSN

    banksel	TRISC	; I2C pins are input
    bsf		TRISC,TRISC_TRISC0_POSN
    bsf		TRISC,TRISC_TRISC1_POSN
    
    return

    ;============  Write to slave ==================
I2C_WriteToSlave:   ; value to send is in WReg on entry
    movwf	bufferValue
    banksel	SSP1CON2 
    movlw	1<<nextWrite | 1<<lastWrite ; are we in a write sequence
    andwf	I2C_control,w		    
    skipz	
    goto	writeData		; yes, just send next byte
    skipIfNotRestartInProgress
;    btfsc	I2C_control, restart    ; bypass START if we're in a restart
    goto	setWriteAddress
    call	waitMSSPIdle	; Wait for I2C bus idle

sendWriteSTART:
    bsf		SSP1CON2,SSP1CON2_SEN_POSN  ; Generate START Condition
    btfsc	SSP1CON2,SSP1CON2_SEN_POSN  ; wait for SEN to clear
    goto	$-1
    
setWriteAddress:
    unsetRestartInProgress
    movf	slaveAddress,w	; send address, shifted left
    lslf	WREG,f
    bcf		WREG, 0		; unset R/W bit = Write
    movwf	SSP1BUF
    call	waitMSSPIdle	; Wait for I2C operation to complete
    btfsc	SSP1CON2,SSP1CON2_ACKSTAT_POSN	
    goto	handleFail				

writeData:
    movf	bufferValue,w	; send value to slave
    movwf	SSP1BUF 
    call	waitMSSPIdle	; Wait for I2C operation to complete
    btfsc	SSP1CON2,SSP1CON2_ACKSTAT_POSN	
    goto	handleFail
    skipIfNextWrite
;    btfss	I2C_control, nextWrite	; in a write sequence?
    goto	checkFirstWrite
    clrf	I2C_control
    retlw	1
    
checkFirstWrite:    
    skipIfFirstWrite
    goto	checkLastWrite
    clrf	I2C_control
    retlw	1
    
checkLastWrite:    
    skipIfLastWrite
    goto	sendWriteSTOP
    unsetLastWrite
    skipIfNotRestart
    goto	sendWriteRESTART
    
sendWriteSTOP:
    banksel	SSP1CON2
    bsf		SSP1CON2,SSP1CON2_PEN_POSN	; Send STOP condition
    call	waitMSSPIdle			; Wait for I2C operation to complete
    clrf	I2C_control
    retlw	1
    
sendWriteRESTART:
    setRestartInProgress		; flag that restart is in progress
    unsetRestart
    bsf		SSP1CON2,SSP1CON2_RSEN_POSN	; Send RESTART condition
    call	waitMSSPIdle			; Wait for I2C operation to complete
    clrf	I2C_control
    retlw	1
    
    ;============  Read from slave ==================
I2C_ReadFromSlave:	; read value from slave, save in bufferValue
    banksel	SSP1CON2 
    ; if this is is a next or last read, then bypass address setting
    skipIfNotNextOrLastRead
    goto	readData
    skipIfRestartInProgress	; bypass the START
    goto	sendReadSTART
    goto	setReadAddress
    
sendReadSTART:
    call	waitMSSPIdle	; Wait for I2C bus idle
    bsf		SSP1CON2,SSP1CON2_SEN_POSN  ; Generate START Condition
    call	waitMSSPIdle	; Wait for START to clear
    
setReadAddress:
    unsetRestartInProgress
    movf	slaveAddress,w	; send address, shifted left
    lslf	WREG,f
    bsf		WREG, 0		; set R/W bit = Read
    movwf	SSP1BUF
    call	waitMSSPIdle	; Wait for I2C operation to complete
    btfss	SSP1CON2,SSP1CON2_ACKSTAT_POSN	
    goto	readData
    clrf	I2C_control
    retlw	0				
    
readData:
    bsf		SSP1CON2,SSP1CON2_RCEN_POSN
    call	waitMSSPIdle	; Wait for I2C operation to complete
    
    movf	SSP1BUF,w	; read and store value sent by slave
    movwf	bufferValue

    ; if first or next read, then ACK, otherwise NACK
    skipIfFirstOrNextRead
    goto	sendNACK
   
sendACK:
    bcf		SSP1CON2,SSP1CON2_ACKDT_POSN ; 0 = ACK
    goto	sendACKorNACK
    
    ; this is either a normal read or a lastRead, so we NACK
sendNACK:			; send NACK to end one read or multiple reads
    bsf		SSP1CON2,SSP1CON2_ACKDT_POSN ; 1 = NACK
    
sendACKorNACK:
    bsf		SSP1CON2,SSP1CON2_ACKEN_POSN ; start (N)ACK sequence
    btfsc	SSP1CON2,SSP1CON2_ACKEN_POSN ; wait for (N)ACK to complete
    goto	$-1
    
    ; determine how to end this interaction, do 'nothing', STOP or RESTART
    skipIfFirstOrNextRead
    goto	checkLastRead
    clrf	I2C_control
    retlw	1		; just return if first or next
 
checkLastRead:
    ; this is either a simple I2C_ReadFromSlave or it is a LAST	read
    skipIfLastRead
    goto	sendReadSTOP
    ; has restart been specified
    skipIfRestart
    goto	sendReadSTOP
    
sendReadRESTART:
    setRestartInProgress
    bsf		SSP1CON2,SSP1CON2_RSEN_POSN	; Send RESTART condition
    call	waitMSSPIdle			; Wait for I2C operation to complete
    clrf	I2C_control
    retlw	1
    
sendReadSTOP:
    bsf		SSP1CON2,SSP1CON2_PEN_POSN	; Send STOP condition
    call	waitMSSPIdle			; Wait for I2C operation to complete
    clrf	I2C_control
    retlw	1
    
handleFail:
    banksel	SSP1CON2
    bsf		SSP1CON2,SSP1CON2_PEN_POSN  ; Send STOP condition
    call	waitMSSPIdle		    
    nop
    clrf	I2C_control
    retlw	0

    ; wait for MSSP request to complete. NB: uses FSR0L as temp register
waitMSSPIdle:
    movf	STATUS,w	; get current bank select bits
    andlw	01100000B	; remove all but the bank select bits
    movwf	FSR0L		; use FSR briefly
    banksel	SSP1STAT
    btfsc	SSP1STAT, SSP1STAT_R_nW_POSN
    goto	$-1
testSSP1CON2:
    movlw	0x1f	    ; all bits must be off
    andwf	SSP1CON2,w
    skipz
    goto	testSSP1CON2
    ; restore the bank select bits
    movlw	10011111B
    andwf	STATUS, f	; clear the bank select bits 
    movf	FSR0L, w		; get the old bank select bits
    xorwf	STATUS, f	; restore old bank select bits
    return
    
stabilisationWait:
    call	delay1s
    return
    
I2C_SetSlaveAddress:	; slave address in WReg on entry
    movwf	slaveAddress
    return
    
I2C_processMSSPInterrupt:
    banksel	PIR1
    btfss	PIR1, PIR1_SSP1IF_POSN
    goto	checkBusCollision
    banksel	SSP1STAT
    btfss	SSP1STAT, SSP1STAT_R_nW_POSN
    goto	masterSending
    movf	bufferValue,w
    movwf	SSP1BUF
    goto	checkBusCollision
    
masterSending:
    btfss	SSP1STAT, SSP1STAT_D_nA_POSN
    goto	processAddress
    movf	SSP1BUF, w   ; read value from buffer
    movwf	bufferValue ; store received value
    bsf		I2C_control, dataReceived
    goto	checkBusCollision
    
processAddress:
    movf	SSP1BUF	    ; clear the BF flag
    
checkBusCollision:
    banksel	PIR2
    btfss	PIR2, PIR2_BCL1IF_POSN
    goto	clearMSSPInterrupt
    banksel	SSP1BUF
    movf	SSP1BUF,w   ; clear the BF flag
    banksel	PIR2
    bcf		PIR2, PIR2_BCL1IF_POSN
    
clearMSSPInterrupt:
    banksel	PIR1
    bcf		PIR1, PIR1_SSP1IF_POSN
    
I2C_ClockRelease:
    btfsc	I2C_control, dataReceived	; clock is held till data processed
    return
    banksel	SSP1CON1
    bsf		SSP1CON1, SSP1CON1_CKP_POSN
    return

#endif


