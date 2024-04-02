#ifdef BUILD_TIMER_LIB
extrn		d1,d2,d3
global		delay1s,delay5s,delay10s,delay50ms,delay100ms,delay200ms,delay300ms,delay400ms,delay500ms,delay700ms,delay5us
global		delay150ms,delay70ms
#else
global 		d1,d2,d3
extrn		delay1s,delay5s,delay10s,delay50ms,delay100ms,delay200ms,delay300ms,delay400ms,delay500ms,delay700ms,delay5us
extrn		delay150ms,delay70ms
#endif