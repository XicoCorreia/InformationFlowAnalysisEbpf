;; Check if n is a prime number
; n = 7
; is_prime = 1
; for i in range(2, n):
;     if n % i == 0:
;         is_prime = 0
    mov r0, 7        ; n = 7
    mov r1, 2        ; i = 2
    mov r2, 1        ; is_prime = 1
    ja +3
    div r0, r1       ; divide n by i
    mul r1, r0       ; multiply result by i
    cmp r0, r1       ; check if remainder == 0
    je +2            ; if divisible, skip
    mov r2, 0        ; is_prime = 0
    add r1, 1        ; i = i + 1
    jlt r1, r0, +-5  ; while i < n
    exit
