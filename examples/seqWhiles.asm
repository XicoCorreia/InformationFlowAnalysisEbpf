    mov r0, 0
    jge r1, 6, +4       ;; while cond
        add r0, r4       ;  body
        sub r2, 2
        add r1, 1
    ja -5
    mul r2, 3           ; common code
    mov r0, 1

    jeq r0, 7, +3       ;; while cond
        add r3, r2       ;  body
        add r0, 1
    ja -4

    jne r3, 2, +3       ;; while cond
        add r0, 2       ;  body
        add r3, 1
    ja -4
    exit