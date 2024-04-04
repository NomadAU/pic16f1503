#ifdef BUILD_I2C_LIB
    
global		I2C_MasterInit,I2C_SlaveInit,I2C_WriteToSlave,I2C_ReadFromSlave
global		I2C_SetSlaveAddress,I2C_InitI2CPins,I2C_processMSSPInterrupt
global		I2C_ClockRelease
extrn 		slaveAddress,bufferValue,delay1s,I2C_control
    
#else
    
extrn		I2C_MasterInit,I2C_SlaveInit,I2C_WriteToSlave,I2C_ReadFromSlave
extrn		I2C_SetSlaveAddress,I2C_InitI2CPins,I2C_processMSSPInterrupt
extrn		I2C_ClockRelease
global 		slaveAddress,bufferValue,delay1s,I2C_control
    
#endif

#define BAUD	    100		; Desired Baud rate (100K)
#define FOSC_KHZ    16000	; Oscillator Clock in kHz    

; I2C_control bit settings
#define dataReceived		0	
#define firstWrite		1
#define nextWrite		2
#define lastWrite		3
#define restart			4
#define firstRead		5
#define nextRead		6
#define lastRead		7	

WPUA_WPUA7_POSN			equ 0007h
skipIfRestartInProgress MACRO
  btfss	    WPUA,WPUA_WPUA7_POSN
ENDM
skipIfNotRestartInProgress MACRO
  btfsc	    WPUA,WPUA_WPUA7_POSN
ENDM
unsetRestartInProgress MACRO
  bcf	    WPUA,WPUA_WPUA7_POSN
ENDM
setRestartInProgress MACRO
  bsf	    WPUA,WPUA_WPUA7_POSN
ENDM
setRestart MACRO
    bsf		I2C_control, restart 
ENDM
unsetRestart MACRO
    bcf		I2C_control, restart 
ENDM
skipIfRestart MACRO
    btfss	I2C_control, restart
ENDM
skipIfNotRestart MACRO
    btfsc	I2C_control, restart
ENDM
  
setFirstRead MACRO
    bsf		I2C_control, firstRead 
ENDM
unsetFirstRead MACRO
    bcf		I2C_control, firstRead 
ENDM
skipIfFirstRead MACRO
    btfss	I2C_control, firstRead
ENDM
skipIfNotFirstRead MACRO
    btfsc	I2C_control, firstRead
ENDM
    
setNextRead MACRO
    bsf		I2C_control, nextRead 
ENDM
unsetNextRead MACRO
    bcf		I2C_control, nextRead 
ENDM
skipIfNextRead MACRO
    btfss	I2C_control, nextRead
ENDM
skipIfNotNextRead MACRO
    btfsc	I2C_control, nextRead
ENDM

setLastRead MACRO
    bsf		I2C_control, lastRead 
ENDM
unsetLastRead MACRO
    bcf		I2C_control, lastRead 
ENDM
skipIfLastRead MACRO
    btfss	I2C_control, lastRead
ENDM
skipIfNotLastRead MACRO
    btfsc	I2C_control, lastRead
ENDM
    
setFirstWrite MACRO
    bsf		I2C_control, firstWrite 
ENDM
unsetFirstWrite MACRO
    bcf		I2C_control, firstWrite 
ENDM
skipIfFirstWrite MACRO
    btfss	I2C_control, firstWrite
ENDM
skipIfNotFirstWrite MACRO
    btfsc	I2C_control, firstWrite
ENDM

setNextWrite MACRO
    bsf		I2C_control, nextWrite 
ENDM
unsetNextWrite MACRO
    bcf		I2C_control, nextWrite 
ENDM
skipIfNextWrite MACRO
    btfss	I2C_control, nextWrite
ENDM
skipIfNotNextWrite MACRO
    btfsc	I2C_control, nextWrite
ENDM
    
setLastWrite MACRO
    bsf		I2C_control, lastWrite 
ENDM
unsetLastWrite MACRO
    bcf		I2C_control, lastWrite 
ENDM
skipIfLastWrite MACRO
    btfss	I2C_control, lastWrite
ENDM
skipIfNotLastWrite MACRO
    btfsc	I2C_control, lastWrite
ENDM

skipIfFirstOrNextRead MACRO
    movlw	(1<<firstRead) | (1<<nextRead) ; skip if either are set
    andwf	I2C_control,w
    skipnz			; skip if either is set
ENDM
skipIfNotFirstOrNextRead MACRO
    movlw	(1<<firstRead) | (1<<nextRead) ; skip if neither are set
    andwf	I2C_control,w
    skipz			; skip if neither is set
ENDM 
skipIfNextOrLastRead MACRO
    movlw	(1<<nextRead) | (1<<lastRead) ; skip if neither are set
    andwf	I2C_control,w
    skipnz			; skip if neither is set
ENDM
skipIfNotNextOrLastRead MACRO
    movlw	(1<<nextRead) | (1<<lastRead) ; skip if neither are set
    andwf	I2C_control,w
    skipz			; skip if neither is set
ENDM
    
skipIfOK MACRO
    ; test return value in WReg - 0=fail, 1=success
    addlw	0x00 
    skipnz
ENDM

; we also use bit 7 of WPUA (an unused bit) to denote a RESTART has been issued
    
; To transmit a value to slave, master sets slaveAddress and places value
; in the bufferValue register before calling I2C_WriteToSlave.
; To receive a value from the slave, the I2C_ReadFromSlave function is called
; and (if successful) the received value will be returned in bufferValue register.
; Both the I2C_WriteToSlave and I2C_ReadFromSlave functions return either
; 0 (failed) or 1 (successful) in the WReg on return.
;
; The slave implements an ISR and calls the I2C_processMSSPInterrupt function.
; The value read from master will be in the bufferValue register, and the 
; dataReceived bit in flagWord will be set.  The clock will also be held until
; I2C_ClockRelease is called by the slave after processing data.
; The current value in bufferValue will be sent to the master when a 
; read is requested.