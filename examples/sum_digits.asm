;; Find the sum of the digits of 4321
; num = 4321
; sum = 0
; while num > 0:
;     digit = num % 10
;     sum = sum + digit
;     num = num // 10
    mov r0, 4321     ; num = 4321
    mov r1, 0        ; sum = 0
    ja +4
    mov r2, r0       ; temp = num
    mod r2, 10       ; digit = num % 10
    add r1, r2       ; sum = sum + digit
    div r0, 10       ; num = num // 10
    jgt r0, 0, +-5   ; while num > 0
    exit
