    mov r0, 0
        jeq r1, 2, +4       ;; if
            add r0, 1       ;  then
            mov r2, 2
            mov r3, 5
        ja +2
            add r4, 3       ; else
            mov r5, 1
    mov r0, 1               ; common code
    add r2, 3
    exit