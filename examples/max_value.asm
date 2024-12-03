;; Find the maximum value in an array
; array = [3, 7, 2]
; max = array[0]
; for i in range(1, len(array)):
;     if array[i] > max:
;         max = array[i]
    mov r0, 3       ; max = array[0]
    mov r1, 7       ; array[1]
    cmp r1, r0      ; compare array[1] with max
    jle +2          ; if not greater, skip
    mov r0, r1      ; update max to array[1]
    mov r1, 2       ; array[2]
    cmp r1, r0      ; compare array[2] with max
    jle +2          ; if not greater, skip
    mov r0, r1      ; update max to array[2]
    exit
