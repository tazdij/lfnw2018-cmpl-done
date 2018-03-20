; Should the source fix endianess?
; FEED = ED FE 00 00 ; in memory

MOV     @x0100  xFEED
MOV     @x0104  @x0100  x04
HALT
