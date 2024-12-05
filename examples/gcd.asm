;; Find the GCD of 36 and 24
; a = 36, b = 24
; while b != 0:
;     temp = b
;     b = a % b
;     a = temp
    mov r0, 36       ; a = 36
    mov r1, 24       ; b = 24
    ja +5
    mov r2, r1       ; temp = b
    mod r0, r1       ; b = a % b
    mov r1, r0       ; a = temp
    mov r0, r2       ; a = temp
    jne r1, 0, +-5      ; while b != 0
    exit
