; Should the source fix endianess?
; FEED = ED FE 00 00 ; in memory

MOV     @x0100  xFEED           ; jksljflkjdslkajfdlksa f
MOV     @x0104  @x0100  x04
MOV     @x0108  x41
;MOV    R1      x41
PRINTC  @x0108
HALT


add_i:
    ARG
    POPI    

main:
    PUSHI   x40
    PUSHI   x01
    CALL    add_i   x02
    POPI    R1          ; Get the result from the stack into a register
    PRINTC  
