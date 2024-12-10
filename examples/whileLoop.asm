    mov r0, 0
    jne r1, 2, +4   ;; while cond

    add r0, 1       ;  body
    mov r2, 2
    mov r3, 5
    ja -5

    add r2, 3       ; common code
    mov r3, 1
    exit