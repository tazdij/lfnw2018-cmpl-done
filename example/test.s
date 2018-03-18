; Should the source fix endianess?
; FEED = ED FE 00 00 ; in memory

MOV     @x00000100  x0000FEED
HALT
