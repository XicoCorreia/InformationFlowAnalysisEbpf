    mov r0, 0
    jne r2, 2, +7           ;; while cond
        mov r8, 1           ; body
        mov r9, 1
        jeq r1, 2, +2       ; if 
            add r3, 1       ;  then
            ja +1
            add r4, 3       ; else
        ja -8
    add r2, 3               ; common code
    mov r4, 1
    exit