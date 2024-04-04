#ifdef BUILD_TIMER_LIB
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
#include "Timer.inc"

PSECT   TimerMain,global,class=CODE,delta=2
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; start of 16MHz delay routines ;;;;;;;;;;;;;;;;;;;;;;;
delay5us:
    movlw	0x05
    movwf	d1
delay5us_0:
    decfsz	d1,f
    goto	delay5us_0
    return
    
delay150ms:
    movlw	0xD1
    movwf	d1
    movlw	0x4F
    movwf	d2
    movlw	0x02
    movwf	d3
delay150ms_0:
    decfsz	d1, f
    goto	$+2
    decfsz	d2, f
    goto	$+2
    decfsz	d3, f
    goto	delay150ms_0
    return
	
delay50ms:
    movlw	0x3E
    movwf	d1
    movlw	0x9D
    movwf	d2
delay50ms_0:
    decfsz	d1, f
    goto	$+2
    decfsz	d2, f
    goto	delay50ms_0
    goto	$+1
    nop
    return
		
delay70ms:
    movlw	0xBE
    movwf	d1
    movlw	0xDB
    movwf	d2
delay70ms_0:
    decfsz	d1, f
    goto	$+2
    decfsz	d2, f
    goto	delay70ms_0
    goto	$+1
    nop
    return
    
delay100ms:
    movlw	0x36
    movwf	d1
    movlw	0xE0
    movwf	d2
    movlw	0x01
    movwf	d3
delay100ms_0:
    decfsz	d1, f
    goto	$+2
    decfsz	d2, f
    goto	$+2
    decfsz	d3, f
    goto	delay100ms_0
    nop    
    return

delay200ms:
    movlw	0x6D
    movwf	d1
    movlw	0xBF
    movwf	d2
    movlw	0x02
    movwf	d3
delay200ms_0:
    decfsz	d1, f
    goto	$+2
    decfsz	d2, f
    goto	$+2
    decfsz	d3, f
    goto	delay200ms_0
    return
    
delay300ms:
    movlw	0xA3
    movwf	d1
    movlw	0x9E
    movwf	d2
    movlw	0x03
    movwf	d3
delay300ms_0:
    decfsz	d1, f
    goto	$+2
    decfsz	d2, f
    goto	$+2
    decfsz	d3, f
    goto	delay300ms_0
    goto	$+1
    return

delay400ms:
    movlw	0xDA
    movwf	d1
    movlw	0x7D
    movwf	d2
    movlw	0x04
    movwf	d3
delay400ms_0:
    decfsz	d1, f
    goto	$+2
    decfsz	d2, f
    goto	$+2
    decfsz	d3, f
    goto	delay400ms_0
    nop
    return    
    
delay500ms:
    movlw	0x11
    movwf	d1
    movlw	0x5D
    movwf	d2
    movlw	0x05
    movwf	d3
delay500ms_0:
    decfsz	d1, f
    goto	$+2
    decfsz	d2, f
    goto	$+2
    decfsz	d3, f
    goto	delay500ms_0
    goto	$+1
    goto	$+1
    return

delay700ms:
    movlw	0x7E
    movwf	d1
    movlw	0x1B
    movwf	d2
    movlw	0x07
    movwf	d3
delay700ms_0:
    decfsz	d1, f
    goto	$+2
    decfsz	d2, f
    goto	$+2
    decfsz	d3, f
    goto	delay700ms_0
    goto	$+1
    goto	$+1
    nop
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

delay5s:
    movlw	0xB5
    movwf	d1
    movlw	0x99
    movwf	d2
    movlw	0x2C
    movwf	d3
delay5s_0:
    decfsz	d1, f
    goto	$+2
    decfsz	d2, f
    goto	$+2
    decfsz	d3, f
    goto	delay5s_0
    goto	$+1
    goto	$+1
    return    
    
delay10s:
    movlw	0x6C
    movwf	d1
    movlw	0x32
    movwf	d2
    movlw	0x58
    movwf	d3
delay10s_0:
    decfsz	d1, f
    goto	$+2
    decfsz	d2, f
    goto	$+2
    decfsz	d3, f
    goto	delay10s_0
    goto	$+1
    nop
    return
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end of 16MHz delay routines ;;;;;;;;;;;;;;;;;;;;;;;
END 
    
#endif