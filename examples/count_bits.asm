;; Count the number of 1 bits in 13
; num = 13
; count = 0
; while num > 0:
;     if num & 1:
;         count = count + 1
;     num = num >> 1
    mov r0, 13       ; num = 13
    mov r1, 0        ; count = 0
    ja +4
    mov r2, r0       ; temp = num
    and r2, 1        ; temp = num & 1
    jeq r2, 0, +2        ; skip if temp == 0
    add r1, 1        ; count = count + 1
    mov r1, 2       ; Load the divisor (2) into r1
    div r0, r1      ; Divide r0 by r1 (i.e., r0 / 2) and store the result back in r0
    jgt r0, 0, +-5   ; while num > 0
    exit
