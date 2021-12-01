section .text

global strlen
global atoi
global itoa
global intlen

; strlen returns the length of a null terminated string
; rax must contain the address of the first char
; 
; rax will contain the length of the string
strlen:
    mov rdi, rax             ; free rax by moving the file to rdi
    xor rax, rax             ; set the counter to 0
    
strlen_check:
    cmp [rdi], byte 0        ; check for null byte
    jz done

    inc rax
    inc rdi
    jmp strlen_check


; intlen returns the amount of bytes `itoa` would need to store the string.
; rax should contain the int and will also contain the result
intlen:
    xor rdi, rdi            ; clear rdi
    mov rcx, 10

    inc rdi                 ; start with 1
.intlen_devide:
    cmp rax, 10             ; check if done
    jl .intlen_done         ; done

    xor	rdx,	rdx         ; clear remainder
    div rcx                 ; devide by 10
    inc rdi                 ; increment digits required
    jmp .intlen_devide

.intlen_done:
    mov rax, rdi
    ret

; convert int to ascii. rax must contain the int. rdi must point to a large enough buffer
; the length required can be calculated with intlen
itoa:
    push rax                ; save rax
    push rdi                ; save rdi

    call intlen             ; calulcate buffer size
    
    pop rdi                 ; pop rdi back
    add rdi, rax            ; increment rdi with the amount of buffer space needed
    dec rdi                 ; off by one

    pop rax                 ; pop rax back

    mov rcx, 10             ; set devider
.itoa_devide:
    xor	rdx,	rdx         ; clear remainder
    div rcx                 ; devide rax by 10

    add rdx, 48             ; convert to ascii
    mov [rdi], dl           ; write lower byte of remainder to buffer
    dec rdi                 ; move pointer

    cmp rax, 0              ; check if done
    jne .itoa_devide

    ret

; atoi takes a string in rax and converts it into an integer. 
; if rdi is 0 a null terminated string is used.
; if rdi is > 0 the string is assumed to be rdi length.
; The result is also placed in rax
atoi:
    mov rdx, rdi            ; save rdx
    add rdx, rax            ; calculate the end address of the string
    
    mov rdi, rax            ; save rax 
    xor rax, rax            ; Set initial total to 0

     
atoi_convert:
        movzx rsi, byte [rdi]               ; Get the current character

        test rdx, rdx                       ; null terminated?
        je .atoi_convert_null_terminated

        cmp rdx, rdi                        ; check if end of string
        je done                             ; done
        jmp .atoi_convert_calculate

.atoi_convert_null_terminated:
        test rsi, rsi                       ; Check for \0
        je done

.atoi_convert_calculate:
    
        cmp rsi, 48                         ; Anything less than 0 is invalid
        jl error
    
        cmp rsi, 57                         ; Anything greater than 9 is invalid
        jg error
     
        sub rsi, 48                         ; Convert from ASCII to decimal
        imul rax, 10                        ; Multiply total by 10
        add rax, rsi                        ; Add current digit to total
    
        inc rdi                             ; Get the address of the next character
        jmp atoi_convert

error:
        mov rax, -1             ; Return -1 on error
 
done:
        ret                     ; Return total or error code
