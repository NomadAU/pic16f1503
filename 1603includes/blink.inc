#ifdef BUILD_BLINK_LIB
    
global		blinkLSBStatusBits,blinkWReg,blink2,blinkLED,blinkLEDnTimes
global		shortFlash,longFlash,quickBlinkLEDnTimes
extrn 		blinkByte,blinkCount,bitCount,colourWord
extrn		turnOnRedLED,turnOffRedLED,turnOnGreenLED,turnOffGreenLED
extrn		turnOnOrangeLED,turnOffOrangeLED
extrn		delay100ms,delay200ms,delay1s,delay50ms,delay500ms
    
#else
    
extrn		blinkLSBStatusBits,blinkWReg,blink2,blinkLED,blinkLEDnTimes
extrn		shortFlash,longFlash,quickBlinkLEDnTimes
global 		blinkByte,blinkCount,bitCount,colourWord
global		turnOnRedLED,turnOffRedLED,turnOnGreenLED,turnOffGreenLED
global		turnOnOrangeLED,turnOffOrangeLED
global		delay100ms,delay200ms,delay1s,delay50ms,delay500ms

#endif
