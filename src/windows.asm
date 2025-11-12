extern _ExitProcess@4
extern _GetStdHandle@4
extern _WriteFile@20
extern _ReadFile@20

global _start
NEWLINE equ 10
STDIN equ 0
STDOUT equ 1

section .data
    prompt1 db "Input a signed value (at most 128 bits) to convert or hit Enter to exit.", 13, 10, \
                "Use 0x for hexadecimal, 0b for binary, and no prefix for decimal: "
    p1Len equ $-prompt1
    prompt2 db "Enter the number of bits this value uses or hit Enter to exit: "
    p2Len equ $-prompt2
    invalidInput db "Invalid input", 13, 10, 13, 10
    invalidLen equ $-invalidInput
    exitMsg db "Thank you for using our number conversion tool!", 13, 10
    exitLen equ $-exitMsg
    decimalMsg db "Decimal: "
    decLen equ $-decimalMsg
    binaryMsg db "Binary: "
    binLen equ $-binaryMsg
    hexMsg db "Hexadecimal: "
    hexLen equ $-hexMsg
    newLine db 13, 10
    asciiDigits db "0123456789abcdef"

    absSignedIntVal dd 0,0,0,0              ; 128-bit integer
    unsignedIntVal dd 0,0,0,0
    tempVal1 dd 1,0,0,0                     ; temp storage for 128-bit ints
    zero dd 0,0,0,0
    maxValSigned dd 1,0,0,0                 ; to help with calculating 2's complement
    maxValUnsigned dd 0,0,0,0
    intValSign db 0

section .bss
    bytesRead resd 1
    input1 resb 1024
    i1Len resb 1
    input2 resb 3                           ; max = 128
    i2Len resb 1
    flushBuf resb 100
    radix resb 1
    numBits resb 1
    printBuf resb 132                       ; for printing other radices

section .text
_start:
    call _reset_data                        ; clear data from previous inputs

    mov ebx, prompt1                        ; print first prompt
    mov ecx, p1Len
    call _windows_write
s:
    mov ebx, input1                         ; read first input (signed int)
    mov ecx, 130
    call _windows_read
    mov [i1Len], eax
    cmp word [input1 + eax - 1], NEWLINE
    jz cont1
    call _flush_stdin                       ; clear stdin

cont1:
    cmp byte [i1Len], 1                     ; exit if end of input or newline
    jbe exit
    cmp byte [input1 + 1], NEWLINE
    jz exit

    movzx esi, byte [i1Len]                 ; if last char is newline, decrease length (-2 for \r\n)
    dec esi
    cmp byte [input1 + esi], NEWLINE
    jnz val1
    sub byte [i1Len], 2

val1:
    call _validate1                         ; validate first input
    cmp eax, 1
    jz _start

    mov ebx, prompt2                        ; print second prompt
    mov ecx, p2Len
    call _windows_write

    mov ebx, input2                         ; read second input (numBits)
    mov ecx, 3
    call _windows_read
    mov [i2Len], eax
    cmp byte [input2 + eax - 1], NEWLINE
    jz cont2
    call _flush_stdin                       ; clear stdin

cont2:
    cmp byte [i2Len], 1                     ; exit if end of input or newline
    jbe exit
    cmp byte [input2 + 1], NEWLINE
    jz exit

    movzx esi, byte [i2Len]                 ; if last char is '\n', decrease length (-2 for \r\n)
    dec esi
    cmp byte [input2 + esi], NEWLINE
    jnz val2
    sub byte [i2Len], 2

val2:
    call _validate2                         ; validate second input
    cmp eax, 1
    jz _start
val3:
    call _validate3                         ; validate if first input works with given # of bits and convert into integer (unsignedIntVal)
    cmp eax, 1
    jz _start

print_dec:
    cmp byte [radix], 1
    jz print_bin
    mov ebx, decimalMsg
    mov ecx, decLen
    call _windows_write
    call _print_decimal
print_bin:
    cmp byte [radix], 2
    jz print_hex
    mov ebx, binaryMsg
    mov ecx, binLen
    call _windows_write
    call _print_binary
print_hex:
    cmp byte [radix], 3
    jz restart
    mov ebx, hexMsg
    mov ecx, hexLen
    call _windows_write
    call _print_hexadecimal
restart:
    mov ebx, newLine
    mov ecx, 2
    call _windows_write
    jmp _start

exit:
    mov ebx, exitMsg
    mov ecx, exitLen
    call _windows_write

    push 0
    call _ExitProcess@4

; ===================================================================================
;                                     MAIN FUNCTIONS
; ===================================================================================

; ----------------------------------------------------------------------------
; Validate first user input
;  Input: 
;   - input1 = user input
;   - i1Len = input length
;  Output: 
;   - radix = 1 for decimal, 2 for binary, 3 for hexadecimal
;   - eax = return code (0 = success, 1 = error)
;   - If invalid, print error message (input not a valid decimal / binary / 
;      hexadecimal signed integer)
;   - In input1, any a-f digit gets converted to lowercase
; ----------------------------------------------------------------------------
_validate1:
    cmp byte [i1Len], 3                     ; check radix
    jb decimal1
    cmp word [input1], "0b"
    jz binary1
    cmp word [input1], "0x"
    jz hexadecimal1

decimal1:
    mov byte [radix], 1
    mov esi, 0
loop_d1:
    mov al, byte [input1 + esi]
    cmp al, "-"                             ; if '-', check if first char
    jz neg_check
    cmp al, "0"                             ; check if '0' <= char <= '9'
    jb val1_fail
    cmp al, "9"
    ja val1_fail
    jmp loop_d1_inc
neg_check:
    cmp esi, 0
    jnz val1_fail
loop_d1_inc:
    inc esi
    movzx eax, byte [i1Len]
    cmp esi, eax
    jb loop_d1
    jmp val1_ret

binary1:
    mov byte [radix], 2
    mov esi, 2
    mov ebx, 0                              ; flag to check if at least one digit has been read
loop_b1:
    mov al, byte [input1 + esi]
    cmp al, "'"                             ; check if ' is not at first digit
    jz delim_check
    cmp al, "0"                             ; check if '0' <= char <= '1'
    jb val1_fail
    cmp al, "1"
    ja val1_fail
    mov ebx, 1
    jmp loop_b1_inc
delim_check:
    cmp esi, 2
    jz val1_fail
loop_b1_inc:
    inc esi
    movzx eax, byte [i1Len]
    cmp esi, eax
    jb loop_b1
    cmp ebx, 0
    jz val1_fail
    jmp val1_ret

hexadecimal1:
    mov byte [radix], 3
    mov esi, 2
loop_h1:
    mov al, byte [input1 + esi]
    call _to_lower_case
    mov byte [input1 + esi], al
    cmp al, "0"                             ; check if '0' <= char <= '9'
    jb check_letter
    cmp al, "9"
    ja check_letter
    jmp loop_h1_inc
check_letter:
    cmp al, "a"                             ; check if 'a' <= char <= 'f' (case-insensitive)
    jb val1_fail
    cmp al, "f"
    ja val1_fail
loop_h1_inc:
    inc esi
    movzx eax, byte [i1Len]
    cmp esi, eax
    jb loop_h1
    
val1_ret:
    mov eax, 0                              ; return 0
    ret

val1_fail:
    call _invalid
    ret


; ----------------------------------------------------------------------------
; Validate second user input
;  Input:
;   - input2 = user input
;   - i2Len = input length
;  Output:
;   - numBits = number of bits integer takes up
;   - eax = return code (0 = success, 1 = error)
;   - If invalid, print error message (!(0 <= input <= 128))
; ----------------------------------------------------------------------------
_validate2:
    mov eax, 0
    mov esi, 0
loop_val2:
    mov cl, byte [input2 + esi]
    cmp cl, "0"                             ; check if '0' <= char <= '9'
    jb val2_fail
    cmp cl, "9"
    ja val2_fail

    sub byte [input2 + esi], "0"            ; convert ascii to integer
    mov dl, 10
    mul dl
    jc val2_fail                            ; overflow (al > 255)
    add al, byte [input2 + esi]
    jc val2_fail

    inc esi
    movzx ecx, byte [i2Len]
    cmp esi, ecx
    jb loop_val2

    cmp al, 128                             ; check if al <= 128 (loop guarantees 0 <= al <= 255)
    ja val2_fail
    cmp al, 0
    jz val2_fail

    mov byte [numBits], al
    mov eax, 0
    ret

val2_fail:
    call _invalid
    ret


; ----------------------------------------------------------------------------
; Check if inputted number is valid signed integer for given number of bits
;  Input:
;   - input1 = signed integer
;   - numBits = # of bits
;  Output:
;   - intVal = value of input1
;   - eax = return code (0 = success, 1 = error)
;   - If invalid, print error message (input1 isn't a valid signed int
;      in range [-2^(numBits-1), 2^(numBits-1) - 1])
; ----------------------------------------------------------------------------
_validate3:
    cmp byte [radix], 1                     ; compute unsigned value
    jz decimal3
    cmp byte [radix], 2
    jz binary3
    cmp byte [radix], 3
    jz hexadecimal3
    jmp val3_fail                           ; shouldn't happen but just in case

decimal3:
    mov edi, 0
    cmp byte [input1], "-"                  ; skip sign
    jz loop_d3_inc
loop_d3:
    mov esi, unsignedIntVal                 ; mul128(esi, ebx) = esi * ebx
    mov ebx, 10
    push edi
    call _mul128
    cmp eax, 1
    jz val3_fail
    pop edi

    mov esi, unsignedIntVal                 ; add128(esi, ebx) = esi + ebx
    movzx ebx, byte [input1 + edi]
    sub ebx, "0"
    push edi
    call _add128
    cmp eax, 1
    jz val3_fail
    pop edi
    
loop_d3_inc:
    inc edi
    movzx eax, byte [i1Len]
    cmp edi, eax
    jl loop_d3
    jmp bound_check


binary3:                                    ; not checking for overflow here since its impossible (user input limited to 0b + 128 bits -> cant overflow intVal)
    movzx edi, byte [i1Len]
    dec edi
loop_b3:
    cmp byte [input1 + edi], "1"
    jnz loop_b3_mul
    push edi
    mov esi, unsignedIntVal
    mov edi, tempVal1
    call _add2_128
    cmp eax, 1
    jz val3_fail
    pop edi

loop_b3_mul:
    cmp byte [input1 + edi], "'"
    jz loop_b3_inc
    mov esi, tempVal1
    mov ebx, 2
    push edi
    call _mul128
    cmp eax, 1
    jz val3_fail
    pop edi
loop_b3_inc:
    dec edi
    cmp edi, 2
    jae loop_b3
    jmp bound_check


hexadecimal3:
    mov edi, 2
loop_h3:
    mov esi, unsignedIntVal                 ; mul128(esi, ebx) = esi * ebx
    mov ebx, 16
    push edi
    call _mul128
    cmp eax, 1
    jz val3_fail
    pop edi

    mov esi, unsignedIntVal
    movzx ebx, byte [input1 + edi]
    cmp byte [input1 + edi], "9"            ; add128(esi, ebx) = esi + ebx
    ja loop_h3_letter
    sub ebx, "0"
    jmp loop_h3_cont2
loop_h3_letter:
    sub ebx, "a"
    add ebx, 10
loop_h3_cont2:
    push edi
    call _add128
    cmp eax, 1
    jz val3_fail
    pop edi

loop_h3_inc:
    inc edi
    movzx eax, byte [i1Len]
    cmp edi, eax
    jb loop_h3


bound_check:
    mov esi, unsignedIntVal
    mov edi, zero
    call _cmp128
    jz bc_loop                              ; treat -0 as just 0 with no sign
    call _check_sign
    mov edi, 0
bc_loop:
    movzx eax, byte [numBits]               ; moved check here since shouldnt loop at all if numBits = 1
    dec eax
    cmp edi, eax
    jae bc_loop_end
    mov esi, maxValSigned                   ; compute signed upper bound
    mov ebx, 2
    push edi
    call _mul128
    pop edi
    inc edi
    jmp bc_loop
bc_loop_end:
    mov esi, maxValSigned                   ; 2 ^ (numBits-1) - 1
    mov ebx, 1
    call _sub128

    mov esi, maxValUnsigned                 ; compute unsigned upper bound
    mov edi, maxValSigned
    call _add2_128
    mov esi, maxValUnsigned                 ; 2 ^ numBits - 2
    mov ebx, 2
    call _mul128
    mov esi, maxValUnsigned                 ; 2 ^ numBits - 1
    mov ebx, 1
    call _add128

    mov esi, unsignedIntVal                 ; if val > unsigned upper bound, even 2's complement won't fit
    mov edi, maxValUnsigned
    call _cmp128
    ja val3_fail

    mov esi, absSignedIntVal
    mov edi, unsignedIntVal
    call _mov128
    cmp byte [intValSign], 1
    jz bc_negcheck
    mov esi, absSignedIntVal                ; check if positive int is > signed max bound
    mov edi, maxValSigned
    call _cmp128
    jg val3_fail
    jmp val3_ret

bc_negcheck:
    cmp byte [radix], 1
    jz bc_negcheck_dec
    mov esi, absSignedIntVal                ; 2's complement
    mov edi, maxValUnsigned
    call _sub2_128
    mov esi, absSignedIntVal
    mov ebx, 1
    call _sub128
    mov esi, absSignedIntVal                ; check if negative int is negative after doing 2's complement
    mov edi, tempVal1
    mov dword [tempVal1], 0
    mov dword [tempVal1 + 4], 0
    mov dword [tempVal1 + 8], 0
    mov dword [tempVal1 + 12], 0
    call _cmp128
    jge val3_fail
    mov esi, tempVal1                       ; get absolute value (0 - neg = pos)
    mov edi, absSignedIntVal
    call _sub2_128
    mov esi, absSignedIntVal
    mov edi, tempVal1
    call _mov128
    jmp val3_ret
bc_negcheck_dec:
    mov esi, maxValSigned                   ; check if abs(intVal) <= abs(signed lower bound)
    mov ebx, 1
    call _add128
    mov esi, absSignedIntVal
    mov edi, maxValSigned
    call _cmp128
    ja val3_fail

val3_ret:
    mov eax, 0
    ret

val3_fail:
    call _invalid
    ret


; ----------------------------------------------------------------------------
; Print signed integer in decimal
;  Input:
;   - absSignedIntVal = absolute value of signed integer
;   - intValSign = 0 if positive, 1 if negative
;  Output:
;   - Prints signed decimal integer
; ----------------------------------------------------------------------------
_print_decimal:
    mov ecx, 0                              ; number of digits
    mov esi, tempVal1
    mov edi, absSignedIntVal
    call _mov128
printd_loop:                                ; get all digits
    push ecx
    mov esi, tempVal1
    mov ebx, 10
    call _div128
    pop ecx
    push edx

    push ecx
    mov esi, tempVal1
    mov edi, zero
    call _cmp128
    pop ecx
    pushfd
    inc ecx
    popfd
    jnz printd_loop

    mov esi, 0                              ; index in printBuf
    cmp byte [intValSign], 1
    jnz printd_pop_loop
    mov byte [printBuf], "-"
    inc esi
printd_pop_loop:                            ; attach digits to printBuf
    pop edx
    mov al, byte [asciiDigits + edx]        ; convert to ascii
    mov byte [printBuf + esi], al 
    inc esi
    dec ecx
    cmp ecx, 0
    jnz printd_pop_loop
    mov byte [printBuf + esi], 13
    inc esi
    mov byte [printBuf + esi], 10
    inc esi

    mov ebx, printBuf                       ; print signed decimal
    mov ecx, esi
    ret


; ----------------------------------------------------------------------------
; Print signed integer in binary
;  Input:
;   - unsignedIntVal = unsigned value calculated from input
;   - intValSign = 0 if positive, 1 if negative
;   - numBits = number of bits integer is to be stored in
;   - maxValUnsigned = 2 ^ numBits - 1
;  Output:
;   - Prints signed binary integer
; ----------------------------------------------------------------------------
_print_binary:
    mov word [printBuf], "0b"
    movzx ecx, byte [numBits]
    add ecx, 4                              ; 0,b,\r,\n
    push ecx                                ; string length
    dec ecx                                 ; start index at end of string
    mov byte [printBuf + ecx], 10
    dec ecx
    mov byte [printBuf + ecx], 13
    dec ecx

    push ecx
    call _convert_unsigned_intval
    pop ecx

printb_loop1:                               ; sign extend
    push ecx
    mov esi, tempVal1
    mov edi, zero
    call _cmp128
    jnz printb_loop2
    pop ecx
    cmp byte [intValSign], 1
    jz set1
    mov byte [printBuf + ecx], "0"
    jmp printb_loop_inc
set1:
    mov byte [printBuf + ecx], "1"
    jmp printb_loop_inc
printb_loop2:                               ; divide by 2, digit = remainder
    mov esi, tempVal1
    mov ebx, 2
    call _div128
    pop ecx
    mov al, byte [asciiDigits + edx]
    mov byte [printBuf + ecx], al
printb_loop_inc:
    dec ecx
    cmp ecx, 1
    jnz printb_loop1
    
    mov ebx, printBuf                       ; print signed binary
    pop ecx
    call _windows_write
    ret


; ----------------------------------------------------------------------------
; Print signed integer in hexadecimal
;  Input:
;   - unsignedIntVal = unsigned value calculated from input
;   - intValSign = 0 if positive, 1 if negative
;   - numBits = number of bits integer is to be stored in
;   - maxValUnsigned = 2 ^ numBits - 1
;  Output:
;   - Prints signed hex integer
; ----------------------------------------------------------------------------
_print_hexadecimal:
    mov word [printBuf], "0x"
    mov edx, 0
    movzx eax, byte [numBits]
    mov ecx, 4
    div ecx
    mov ecx, eax                            ; numDigits = numBits/4 rounded up
    cmp edx, 0
    jz printh_cont1
    inc ecx
printh_cont1:
    add ecx, 4                              ; 0,x,\r,\n
    push ecx
    dec ecx
    mov byte [printBuf + ecx], 10
    dec ecx
    mov byte [printBuf + ecx], 13
    dec ecx

    push ecx
    call _convert_unsigned_intval
    pop ecx

printh_loop1:
    push ecx
    mov esi, tempVal1                       ; sign extend
    mov edi, zero
    call _cmp128
    jnz printh_loop2
    pop ecx
    cmp byte [intValSign], 1
    jz set_negh1
    mov byte [printBuf + ecx], "0"          ; positive -> 0
    jmp printh_loop_inc
set_negh1:                                  ; negative but not at sign bit -> f
    cmp ecx, 2
    jz set_negh2
    mov byte [printBuf + ecx], "f"
    jmp printh_loop_inc
set_negh2:                                  ; negative and at sign bit -> depends on numBits
    movzx ax, byte [numBits]
    mov dl, 4
    div dl
    cmp ah, 0
    jnz set_negh3
    mov byte [printBuf + ecx], "f"
    jmp printh_loop_inc
set_negh3:
    cmp ah, 1
    jnz set_negh4
    mov byte [printBuf + ecx], "1"
    jmp printh_loop_inc
set_negh4:
    cmp ah, 2
    jnz set_negh5
    mov byte [printBuf + ecx], "3"
    jmp printh_loop_inc
set_negh5:
    mov byte [printBuf + ecx], "7"
    jmp printh_loop_inc
printh_loop2:
    mov esi, tempVal1
    mov ebx, 16
    call _div128
    pop ecx
    mov al, byte [asciiDigits + edx]
    mov byte [printBuf + ecx], al
printh_loop_inc:
    dec ecx
    cmp ecx, 1
    jnz printh_loop1

    mov ebx, printBuf
    pop ecx
    call _windows_write
    ret


; ===================================================================================
;                                   HELPER FUNCTIONS
; ===================================================================================

; ----------------------------------------------------------------------------
; Convert char into lowercase
;  Input: 
;   - al = char to convert
;  Output:
;   - al = lowercase char (if al not in range [A-B], nothing changes)
; ----------------------------------------------------------------------------
_to_lower_case:
    cmp al, "A"
    jb tlc_ret
    cmp al, "Z"
    ja tlc_ret
    add al, 32
tlc_ret:
    ret


; ----------------------------------------------------------------------------
; Print error message for invalid input
;  Output: 
;   - eax = 1
; ----------------------------------------------------------------------------
_invalid:
    mov ebx, invalidInput
    mov ecx, invalidLen
    call _windows_write
    mov eax, 1
    ret


; ----------------------------------------------------------------------------
; Empty stdin
; ----------------------------------------------------------------------------
_flush_stdin:
    mov ebx, flushBuf
    mov ecx, 100
    call _windows_read
    cmp eax, 100
    jnz fs_end
    cmp byte [flushBuf + 99], NEWLINE
    jz _flush_stdin
fs_end:
    ret


; ----------------------------------------------------------------------------
; Reset data used for validation
; ----------------------------------------------------------------------------
_reset_data:
    mov dword [absSignedIntVal], 0                   ; clear 128-bit data before computation
    mov dword [absSignedIntVal + 4], 0
    mov dword [absSignedIntVal + 8], 0
    mov dword [absSignedIntVal + 12], 0
    mov dword [unsignedIntVal], 0
    mov dword [unsignedIntVal + 4], 0
    mov dword [unsignedIntVal + 8], 0
    mov dword [unsignedIntVal + 12], 0
    mov dword [tempVal1], 1
    mov dword [tempVal1 + 4], 0
    mov dword [tempVal1 + 8], 0
    mov dword [tempVal1 + 12], 0
    mov dword [zero], 0
    mov dword [zero + 4], 0
    mov dword [zero + 8], 0
    mov dword [zero + 12], 0
    mov dword [maxValSigned], 1
    mov dword [maxValSigned + 4], 0
    mov dword [maxValSigned + 8], 0
    mov dword [maxValSigned + 12], 0
    mov dword [maxValUnsigned], 0
    mov dword [maxValUnsigned + 4], 0
    mov dword [maxValUnsigned + 8], 0
    mov dword [maxValUnsigned + 12], 0
    mov byte [intValSign], 0
    ret


; ----------------------------------------------------------------------------
; Add to a 128-bit number
;  Input:
;   - esi = pointer to 128-bit number in memory (number should be stored in 
;            4 doubleword chunks in little endian order)
;   - ebx = number to add (must be <= 0xFFFFFFFF)
;  Output:
;   - 128-bit number in memory contains sum
;   - eax = return code (0 = no overflow, 1 = overflow)
; ----------------------------------------------------------------------------
_add128:
    mov edi, 0                              ; loop counter
add_loop:
    mov eax, [esi]
    add eax, ebx
    mov ebx, 0                              ; set ebx to carry
    adc ebx, 0
    mov [esi], eax

    add esi, 4
    inc edi
    cmp edi, 4
    jl add_loop

    test ebx, ebx
    jz add_ret
    mov eax, 1
    ret

add_ret:
    mov eax, 0
    ret


; ----------------------------------------------------------------------------
; Add 2 128-bit numbers
;  Input:
;   - esi = pointer to 128-bit number in memory (number should be stored in 
;            4 doubleword chunks in little endian order)
;   - edi = pointer to other 128-bit number in memory
;  Output:
;   - Number pointed to by esi contains sum
;   - eax = return code (0 = no overflow, 1 = overflow)
; ----------------------------------------------------------------------------
_add2_128:
    mov ecx, 0                              ; loop counter
    mov edx, 0                              ; previous carry
add2_loop:
    push ecx                                ; using ecx to temporarily store next carry
    mov eax, [esi]
    mov ebx, [edi]
    add eax, ebx
    mov ecx, 0                              ; store next carry
    adc ecx, 0
    add eax, edx                            ; add previous carry
    adc ecx, 0
    mov edx, ecx                            ; edx = next carry
    mov [esi], eax
    pop ecx

    add esi, 4
    add edi, 4
    inc ecx
    cmp ecx, 4
    jl add2_loop

    test edx, edx
    jz add2_ret
    mov eax, 1
    ret

add2_ret:
    mov eax, 0
    ret


; ----------------------------------------------------------------------------
; Subtract from a 128-bit number
;  Input:
;   - esi = pointer to 128-bit number in memory (number should be stored in 
;            4 doubleword chunks in little endian order)
;   - ebx = number to subtract by (must be <= 0xFFFFFFFF)
;  Output:
;   - 128-bit number in memory contains sum
;   - eax = return code (0 = no overflow, 1 = overflow)
; ----------------------------------------------------------------------------
_sub128:
    mov edi, 0                              ; loop counter
sub_loop:
    mov eax, [esi]
    sub eax, ebx
    mov ebx, 0                              ; set ebx to carry
    adc ebx, 0
    mov [esi], eax

    add esi, 4
    inc edi
    cmp edi, 4
    jl sub_loop

    test ebx, ebx
    jz sub_ret
    mov eax, 1
    ret

sub_ret:
    mov eax, 0
    ret


; ----------------------------------------------------------------------------
; Subtract 2 128-bit numbers
;  Input:
;   - esi = pointer to 128-bit number in memory (number should be stored in 
;            4 doubleword chunks in little endian order)
;   - edi = pointer to other 128-bit number
;  Output:
;   - Number pointed to by esi contains difference
;   - eax = return code (0 = no overflow, 1 = overflow)
; ----------------------------------------------------------------------------
_sub2_128:
    mov ecx, 0                              ; loop counter
    mov edx, 0                              ; previous carry
sub2_loop:
    push ecx                                ; using ecx to temporarily store next carry
    mov eax, [esi]
    mov ebx, [edi]
    sub eax, ebx
    mov ecx, 0                              ; store next carry
    adc ecx, 0
    sub eax, edx                            ; subtract previous carry
    adc ecx, 0
    mov edx, ecx                            ; edx = next carry
    mov [esi], eax
    pop ecx

    add esi, 4
    add edi, 4
    inc ecx
    cmp ecx, 4
    jl sub2_loop

    test edx, edx
    jz sub2_ret
    mov eax, 1
    ret

sub2_ret:
    mov eax, 0
    ret


; ----------------------------------------------------------------------------
; Multiply a 128-bit number (unsigned)
;  Input:
;   - esi = pointer to 128-bit number in memory (number should be stored in 
;            4 doubleword chunks in little endian order)
;   - ebx = multiplier
;  Output:
;   - 128-bit number in memory contains product
;   - eax = return code (0 = no overflow, 1 = overflow)
; ----------------------------------------------------------------------------
_mul128:
    mov ecx, 0                              ; ecx = previous higher bits from imul
    mov edx, 0                              ; edx = higher bits from current imul
    mov edi, 0                              ; edi = loop counter (0 - 3)
mul_loop:
    mov eax, [esi]                          ; eax = current 4 bytes
    mul ebx                                 ; edx:eax = eax * ebx
    add eax, ecx                            ; add previous carry after multiplication
    mov ecx, edx                            ; set next carry
    adc ecx, 0                              ; add CF if eax + ecx overflows
    mov [esi], eax                          ; store result
    
    add esi, 4
    inc edi
    cmp edi, 4
    jl mul_loop
    
    test ecx, ecx                           ; if ecx != 0, then number doesn't fit in 128 bits
    jz mul_ret
    mov eax, 1
    ret

mul_ret:
    mov eax, 0
    ret


; ----------------------------------------------------------------------------
; Divide a 128-bit number (unsigned)
;  Input:
;   - esi = pointer to 128-bit number in memory (number should be stored in 
;            4 doubleword chunks in little endian order)
;   - ebx = divisor
;  Output:
;   - 128-bit number in memory contains quotient
;   - edx = remainder
; ----------------------------------------------------------------------------
_div128:
    mov ecx, 0                                  ; highest to lowest order
    mov edx, 0                                  ; remainder
    add esi, 12
div_loop:
    mov eax, [esi]
    div ebx                                     ; eax/ebx = eax, remainder edx
    mov [esi], eax
    sub esi, 4
    inc ecx
    cmp ecx, 4
    jb div_loop
div_ret:
    ret


; ----------------------------------------------------------------------------
; Compare two 128-bit numbers (unsigned)
;  Input:
;   - esi = pointer to 128-bit number in memory (number should be stored in 
;            4 doubleword chunks in little endian order)
;   - edi = pointer to second 128-bit number in memory
;  Output:
;   - Flags set for cmp esi, edi
; ----------------------------------------------------------------------------
_cmp128:
    mov ecx, 0
    add esi, 12                             ; compare highest bits first
    add edi, 12
cmp_loop:
    mov eax, dword [esi]
    mov ebx, dword [edi]
    cmp eax, ebx
    jnz cmp_ret                             ; difference in bits found

    sub esi, 4
    sub edi, 4
    inc ecx
    cmp ecx, 4
    jl cmp_loop
cmp_ret:
    ret


; ----------------------------------------------------------------------------
; Move a 128-bit number from one location in memory to another
;  Input:
;   - esi = pointer to 128-bit number in memory (destination)
;   - edi = pointer to 128-bit number in memory (source)
;  Output:
;   - Destination memory contains value of source memory
; ----------------------------------------------------------------------------
_mov128:
    mov eax, dword [edi]
    mov [esi], eax
    mov eax, dword [edi + 4]
    mov [esi + 4], eax
    mov eax, dword [edi + 8]
    mov [esi + 8], eax
    mov eax, dword [edi + 12]
    mov [esi + 12], eax
    ret

; ----------------------------------------------------------------------------
; Check sign of input1
;  Input:
;   - input1 = ascii string containing number
;   - i1Len = length of string
;   - radix = number base (1 = decimal, 2 = binary, 3 = hexadecimal)
;   - numBits = number of bits number is to be stored in
;  Output:
;   - intValSign = 0 if positive, 1 if negative
; ----------------------------------------------------------------------------
_check_sign:
    mov byte [intValSign], 0
    cmp byte [radix], 2
    jz b_cs
    cmp byte [radix], 3
    jz h_cs

d_cs:
    cmp byte [input1], "-"
    jnz ret_cs
    mov byte [intValSign], 1
    jmp ret_cs
b_cs:
    mov edi, 2
b_cs_loop:
    cmp byte [input1 + edi], "'"            ; find first non-delimiter char
    jnz b_cs_end
    inc edi
    jmp b_cs_loop
b_cs_end:
    cmp byte [input1 + edi], "1"            ; negative if MSB = 1
    jnz ret_cs
    mov al, byte [i1Len]
    sub al, 2
    cmp al, byte [numBits]                  ; make sure MSB in ascii string is actual MSB
    jb ret_cs
    mov byte [intValSign], 1
    jmp ret_cs
h_cs:
    movzx ax, byte [numBits]
    mov dl, 4
    div dl
    cmp ah, 0                               ; numDigits = numBits/4 rounded up
    jz h_cs_cont
    inc al
h_cs_cont:
    mov cl, byte [i1Len]                    ; if string has less chars than needed to get to sign, skip to end
    sub cl, 2
    cmp al, cl
    jnz ret_cs

    cmp ah, 1
    jz h_cs1
    cmp ah, 2
    jz h_cs2
    cmp ah, 3
    jz h_cs3
h_cs0:
    cmp byte [input1 + 2], "8"
    jb ret_cs
    mov byte [intValSign], 1
    jmp ret_cs
h_cs1:
    cmp byte [input1 + 2], "1"
    jb ret_cs
    mov byte [intValSign], 1
    jmp ret_cs
h_cs2:
    cmp byte [input1 + 2], "2"
    jb ret_cs
    mov byte [intValSign], 1
    jmp ret_cs
h_cs3:
    cmp byte [input1 + 2], "4"
    jb ret_cs
    mov byte [intValSign], 1

ret_cs:
    ret


; ----------------------------------------------------------------------------
; Convert unsigned intval computed from decimal to match what it would be 
; computed from binary/hexadecimal
;  Input:
;   - unsignedIntVal = unsigned integer to convert
;   - maxValUnsigned = 2 ^ numBits - 1
;  Output:
;   - tempVal1 contains converted unsigned integer (if unsignedIntVal wasn't
;      computed from a negative decimal input, tempVal1 == unsignedIntVal)
; ----------------------------------------------------------------------------
_convert_unsigned_intval:
    cmp byte [intValSign], 1                ; if negative and computed from decimal, convert unsignedIntVal to what it should be when computed from binary/hex
    jnz cui_ret
    cmp byte [radix], 1
    jnz cui_ret
    mov esi, tempVal1
    mov edi, zero
    call _mov128
    mov esi, tempVal1
    mov edi, unsignedIntVal
    call _sub2_128
    mov esi, tempVal1
    mov edi, maxValUnsigned
    call _add2_128
    mov esi, tempVal1
    mov ebx, 1
    call _add128
    mov eax, 1
    ret
cui_ret:
    mov esi, tempVal1
    mov edi, unsignedIntVal
    call _mov128
    mov eax, 0
    ret

; ===================================================================================
;                                WINDOWS-SPECIFIC FUNCTIONS
; ===================================================================================

; ----------------------------------------------------------------------------
; Read from stdin (Windows only)
;  Input:
;   - ebx = pointer to buffer to store bytes read
;   - ecx = max number of bytes to read
;  Output:
;   - eax = number of bytes read 
;   - ebx buffer contains data read from stdin
; ----------------------------------------------------------------------------
_windows_read:
    push -10
    call _GetStdHandle@4

    push 0
    push bytesRead
    push ecx
    push ebx
    push eax
    call _ReadFile@20
wr:
    mov eax, dword [bytesRead]
    ret


; ----------------------------------------------------------------------------
; Write to stdout (Windows only)
;  Input:
;   - ebx = pointer to buffer to write
;   - ecx = max number of bytes to write
;  Output:
;   - Message printed to stdout
; ----------------------------------------------------------------------------
_windows_write:
    push -11
    call _GetStdHandle@4

    push 0
    push 0
    push ecx
    push ebx
    push eax
    call _WriteFile@20

    ret