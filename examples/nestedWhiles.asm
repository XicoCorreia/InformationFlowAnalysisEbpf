    mov r0, 0
    jne r2, 2, +7           ;; while cond 1
    add r0, 1               ;  body 1
        jne r1, 2, +4       ;; while cond 2
            add r3, 1       ;  body 2
            mov r4, 2
            mov r5, 5
        ja -5
    ja -8
    add r2, 3               ; common code
    mov r3, 1
    exit
