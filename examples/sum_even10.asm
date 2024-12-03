;; Compute the sum of even numbers from 1 to 10
; sum = 0
; i = 1
; while i <= 10:
;     if i % 2 == 0:
;         sum = sum + i
;     i = i + 1
    mov r0, 0        ; sum = 0
    mov r1, 1        ; i = 1
    ja +3
    mov r2, r1       ; temp = i
    mod r2, 2        ; temp = i % 2
    jne r2, 0, +2    ; skip if not even
    add r0, r1       ; sum = sum + i
    add r1, 1        ; i = i + 1
    jle r1, 10, +-6  ; while i <= 10
    exit
