
 ;Constants
#define SELECT_BANK_0           banksel PORTC 
#define SELECT_BANK_1           banksel TRISA 
#define SELECT_BANK_2           banksel LATA 
#define SELECT_BANK_3           banksel ANSELA 
#define SELECT_BANK_4           banksel WPUA 
#define SELECT_BANK_7           banksel IOCAP 
#define SELECT_BANK_31		banksel	STATUS_SHAD
		
#define OK_COMPLETE		1
#define MATCH			1
#define NO_MATCH		2
#define OK_MORE_DATA		2
#define NOT_OK			3
#define LAST_CHAR		4
#define MORE_CHARS		5

; flagWord bits

#ifndef IO_LINE_PASSING		
#define dataStart		0
#define pinIsOff		0
#define tmr0Expired		1
		
#define keepAliveTimerExpired	1
#define beamBroken		2
#define sendTestMsg		3
		
#define eastTriggerMsg		3
		
#define eastKeepAliveMsg	3
#define timerPopped		4
;#define waitingForKeepAlive	4
#define sendKeepAliveMsg	5
		
#define westTriggerMsg		5
		
#define westKeepAliveMsg	5
;#define LEDOn			5
		

#define inTriggerDelay		6
		
#define isEscapedChar		7
#endif

#ifdef IO_LINE_PASSING		
; flagWord bits for IO line passing implementation
#define eastTriggerPin		0
#define westTriggerPin		1
#define keepAliveTimerExpired	2
#define tmr0Expired		3
		
		
; pin names
#endif
		
; colourWord bits		
#define red			0
#define green			1
		
#define orange			2
#define oneSecondFlashActive	3
#define keepAliveFlashActive	4
#define LEDFlashInProgress	5
#define eastGateIsOn		6
#define westGateIsOn		7

#define quickFlash		7
		
; controlWord bits		
#define EGSwitchedByWebServer	0
#define WGSwitchedByWebServer	1
		
#define ACK_OR_TIMEOUT		(1<<dataStart | 1<<tmr0Expired)

    ; We want a maximum delay of 1 second between sending a message and receiving
    ; an ACK.  61 full TMR0 cycles with 1:256 prescaler is pretty close.
#define TMR0_ACK_WAIT_CYCLES	61	; we'll wait for 61 TMR0 cycles
    ; We want a delay of 2 second between triggering events.  122 full TMR0 
    ; cycles with 1:256 prescaler is pretty close.
#define TMR0_TRIGGER_CYCLES	122	; we'll wait for 122 TMR0 cycles

#define	TMR0_100MS_LED_BLINK_CYCLES 6	; wait for around 100ms
#define TMR0_50MS_LED_BLINK_CYCLES  3	; wait for around 50ms
 
