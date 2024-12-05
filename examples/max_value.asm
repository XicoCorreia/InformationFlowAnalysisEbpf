;; Find the maximum value in an array
; array = [3, 7, 2]
; max = array[0]
; for i in range(1, len(array)):
;     if array[i] > max:
;         max = array[i]
    mov r0, 3       ; max = array[0]
    mov r1, 7       ; array[1]
    sub r1, r0      ; r1 = array[1] - max
    mov r2, r1      ; r2 = array[1] - max
    jlt r2, 0, +2   ; if r2 < 0 jump (r1 is not greater than r0)
    mov r0, r1      ; update max to array[1]
    mov r1, 2       ; array[2]
    sub r1, r0      ; r1 = array[2] - max
    mov r2, r1      ; r2 = array[2] - max
    jlt r2, 0, +2   ; if r2 < 0 jump (r1 is not greater than r0)
    mov r0, r1      ; update max to array[2]
    exit