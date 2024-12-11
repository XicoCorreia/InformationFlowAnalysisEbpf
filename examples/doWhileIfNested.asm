    mov r3, 6
    mov r0, 0
        add r0, 1           ;;  body
        jeq r3, 6, +2       ; if
            add r1, 1       ; then
        ja +1   
            mov r2, 1       ; else
    jne r1, 2, +-6          ; while cond
    
    add r2, 3               ; common code
    mov r3, 1
        mov r0, 1           ;;  body
        jeq r1, 6, +2       ; if
            add r4, 1       ; then
        ja +1   
            mov r5, 1       ; else
    jne r3, 2, +-6          ; while cond
    exit