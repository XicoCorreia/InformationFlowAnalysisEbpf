;; Count the number of 1 bits in 13
; num = 13
; count = 0
; while num > 0:
;     if num & 1:
;         count = count + 1
;     num = num >> 1
    mov r0, 13       ; num = 13
    mov r1, 0        ; count = 0
    mov r2, r0       ; temp = num
    and r2, 1        ; temp = num & 1
    jeq r2, 0, +3    ; Skip if (num & 1) == 0
    add r1, 1        ; count = count + 1
    mov r3, 2        ; Load divisor (2) into r3
    div r0, r3       ; num = num / 2
    jgt r0, 0, +-7    ; Jump back to the start if num > 0
    exit
