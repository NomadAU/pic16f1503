#ifdef BUILD_LIGHT_PROGRAMS_LIB
global		SOS,turnOnStringA,turnOnStringB,toggleStrings,lightsOn,lightsOff
global		initDAC,setLightsHigh,getNextTMR1HValue,processTimer0Expired
global		processTimer1Expired,initialiseTMR0,initialiseTMR1,increasingFlash
global		enableStringSwitch,disableStringSwitch,HOHOHO,merryXmas,increasingIntensity
extrn		flashCounter,TMR1H_index,TMR1H_val,tmr0RolloverCount
extrn		tmr0SecondsCount,programStatus,programCounter1,programCounter2
extrn		currentVoltage
#else
extrn		SOS,turnOnStringA,turnOnStringB,toggleStrings,lightsOn,lightsOff
extrn		initDAC,setLightsHigh,getNextTMR1HValue,processTimer0Expired
extrn		processTimer1Expired,initialiseTMR0,initialiseTMR1,increasingFlash
extrn		enableStringSwitch,disableStringSwitch,HOHOHO,merryXmas,increasingIntensity
global		flashCounter,TMR1H_index,TMR1H_val,tmr0RolloverCount
global		tmr0SecondsCount,programStatus,programCounter1,programCounter2
global		currentVoltage
#endif
    
#define	OFF_DAC_SETTING	    0
#define	LOW_DAC_SETTING	    20
#define	HIGH_DAC_SETTING    31
#define	ON_DAC_SETTING	    31
    
;#define TMR0_VALUE 200  ; count-up from d'100' to 255 = 155 counts
#define TMR0_ROLLOVER_COUNT 63  ; this gives close to 1 second interval
#define TMR0_SECONDS_VALUE 3  
    
; programStatus bits
#define PROGRAM_COMPLETE_BIT	0
#define ENABLE_SWITCH_BIT	1	

SET_TIMER1_COUNTER MACRO	; sets .5 second overflow
    banksel	T1CON	
    movf	TMR1H_val,w
    movwf	TMR1H		    
ENDM
