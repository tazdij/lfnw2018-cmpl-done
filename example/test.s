; Should the source fix endianess?
; FEED = ED FE 00 00 ; in memory

;; Call to Main Function
CALL    main    x00

MOV     @x0100  xFEED           ; jksljflkjdslkajfdlksa f
MOV     @x0104  @x0100  x04
MOV     @x0108  x41
;MOV    R1      x41
PRINTC  @x0108
HALT


add_i:
    ARGI    R1  x01
    ARGI    R2  x02
    ADDI    R1  R2
    PUSHI   R1
    RET     x01
    
main:
    PUSHI   x40
    PUSHI   x01
    CALL    add_i   x02
    POPI    R1          ; Get the result from the stack into a register
    PRINTC
