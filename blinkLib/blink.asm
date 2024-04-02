#ifdef BUILD_BLINK_LIB
; TimerCode:	Collection of logic loops providing specific timing intervals.
;		Categorised according to the oscillator in use.
; Author:	Mike Brady
; Company:	Java Point Pty Ltd
;

; Delay routines for 16MHz clock
PROCESSOR   16F1503
PAGEWIDTH   132
RADIX       DEC

#include <xc.inc>
    
    #include "defines.inc"
    #include "blink.inc"

    PSECT   BlinkMain,global,class=CODE,delta=2

; the value in W reg is analysed as a bitstrip.  A long flash for 1 and a short flash for 0
; Only the 3 LSBs are analysed, Z, DC, C
blinkLSBStatusBits:
    ; on entry, W reg contains byte to inspect
    ; shift left 5 times
    movwf	blinkByte
    rlf		blinkByte, f	    ; move msb to Carry
    rlf		blinkByte, f	    ; move msb to Carry
    rlf		blinkByte, f	    ; move msb to Carry
    rlf		blinkByte, f	    ; move msb to Carry
    rlf		blinkByte, f	    ; move msb to Carry
    movlw	3
    movwf	bitCount
nextStatusBit:
    rlf		blinkByte, f	    ; move msb to Carry
    btfsc	STATUS, STATUS_C_POSN
    goto	statusIsOn
statusIsOff:
    call	shortFlash
    goto	checkStatusEnd
statusIsOn:
    call	longFlash
    goto	checkStatusEnd
checkStatusEnd: 
    decfsz	bitCount, f
    goto	nextStatusBit
    return
    
; the value in W reg is analysed as a bitstrip.  A long flash for 1 and a short flash for 0
blinkWReg:
    ; on entry, W reg contains byte to inspect
    movwf	blinkByte
    movlw	8
    movwf	bitCount
nextBit:
    rlf		blinkByte, f	    ; move msb to Carry
    btfsc	STATUS, STATUS_C_POSN
    goto	isOn
isOff:
    call	shortFlash
    goto	checkEnd
isOn:
    call	longFlash
    goto	checkEnd
checkEnd: 
    call	delay500ms;
    decfsz	bitCount, f
    goto	nextBit
    return
    
shortFlash:
    call	turnOnRedLED
    call	delay100ms	
    call	turnOffRedLED
    call	delay200ms
    return
    
longFlash:
    call	turnOnRedLED
    call	delay1s	
    call	turnOffRedLED
    call	delay200ms
    return

blink2:
    call	blinkLED
    call	delay100ms
    call	blinkLED
    return
    
blinkLED:
;    bsf		DEBUG_LED
    call	turnOnRedLED
    call	delay100ms
    call	turnOffRedLED
    return

;===============================================================================  
; [quick]BlinkLEDnTimes
;
; On entry, number of blinks is in W reg, and colour is in colourWord.
; Default colour is red
;===============================================================================    
blinkLEDnTimes:
    bcf		colourWord, quickFlash
    goto	$+2
quickBlinkLEDnTimes:
    bsf		colourWord, quickFlash
    movwf	blinkCount	; save number of blinks
blinkAgain:
    btfsc	colourWord, red
    goto	blinkOnTheRed
    btfsc	colourWord, green
    goto	blinkOnTheGreen
    btfsc	colourWord, orange
    goto	blinkOnTheOrange
    goto	blinkOnTheRed	    ; default case
    return
blinkOnTheOrange:
    call	turnOnOrangeLED
    goto	blinkDelay
blinkOnTheRed:
    call	turnOnRedLED
    goto	blinkDelay
blinkOnTheGreen:
    call	turnOnGreenLED
blinkDelay:
    btfss	colourWord, quickFlash
    goto	longFlash1
    call	delay50ms
    goto	$+2
longFlash1:
    call	delay1s
    
    btfsc	colourWord, red
    goto	blinkOffTheRed
    btfsc	colourWord, green
    goto	blinkOffTheGreen
    btfsc	colourWord, orange
    goto	blinkOffTheOrange
    goto	blinkOffTheRed	    ; default case
blinkOffTheOrange:
    call	turnOffOrangeLED
    goto	checkForAnotherBlink
blinkOffTheRed:
    call	turnOffRedLED
    goto	checkForAnotherBlink
blinkOffTheGreen:
    call	turnOffGreenLED
    
checkForAnotherBlink:
    btfss	colourWord, quickFlash
    goto	longFlash2
    call	delay50ms
    goto	$+2
longFlash2:
    call	delay1s
    decfsz	blinkCount, f
    goto	blinkAgain
    
    btfss	colourWord, quickFlash
    goto	longFlash3
    call	delay50ms
    goto	$+2
longFlash3:
    call	delay1s
    return

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end of 16MHz baud delay routines ;;;;;;;;;;;;;;;;;;;;;;;
    
    end
#endif