;; Compute the factorial of 5
; factorial = 1
; i = 5
; while i > 1:
;    factorial = factorial * i
;    i = i - 1
    mov r0, 1
    mov r1, 5
    ja +2
    mul r0, r1
    sub r1, 1
    jgt r1, 1, +-3
    exit
