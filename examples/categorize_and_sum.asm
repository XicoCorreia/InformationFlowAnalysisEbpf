;; Categorize and sum positive, negative, and zero elements in an array
; array = [3, -1, 0, 7, -5]
; pos_sum = 0, neg_sum = 0, zero_count = 0
; for each element:
;     if element > 0:
;         pos_sum += element
;     elif element < 0:
;         neg_sum += element
;     else:
;         zero_count += 1

    mov r0, 5        ; array size
    mov r1, 0        ; index = 0
    mov r2, 0        ; pos_sum = 0
    mov r3, 0        ; neg_sum = 0
    mov r4, 0        ; zero_count = 0
    ja +6
    cmp r1, 0        ; if index == 0, load first element
    je +2
    add r5, 4        ; move to the next array element
    ld r6, [r5]      ; load current element into r6
    cmp r6, 0        ; compare current element with 0
    jgt +3           ; if > 0, jump to positive block
    jlt +3           ; if < 0, jump to negative block
    add r4, 1        ; zero_count++
    ja +4            ; jump to loop increment
    add r2, r6       ; pos_sum += element
    ja +2            ; skip to loop increment
    add r3, r6       ; neg_sum += element
    add r1, 1        ; index++
    cmp r1, r0       ; check if index < array size
    jl +-12          ; loop back if more elements
    exit
