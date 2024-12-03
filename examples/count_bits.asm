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
    je r2, +2        ; skip if temp == 0
    add r1, 1        ; count = count + 1
    shr r0, 1        ; num = num >> 1
    jgt r0, 0, +-5   ; while num > 0
    exit
