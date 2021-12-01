extern strlen
extern atoi
extern itoa
extern intlen

section .bss
buffer           resb 512  ; buffer for reading the inputfile
bufferSize       resq 8    ; amount of data read into buffer
inputfileName    resq 8    ; name of the inputfile
inputfile        resq 8    ; file descriptor of inputfile

section .data
    open_failed             db "failed to open file", 10
    open_failed_len         equ $-open_failed
    read_failed             db "read syscall failed", 10
    read_failed_len         equ $-open_failed
    no_newline_found        db "no newline found in buffer", 10
    no_newline_found_len    equ $-no_newline_found
    no_file_provided        db "no file provided", 10
    no_file_provided_len    equ $-no_file_provided
    atoi_failed             db "atoi failed", 10
    atoi_failed_len         equ $-atoi_failed
    test_input              db "193", 0
    

section .text
global _start

_start:
    mov r12, [rsp]                      ; load argc into r12
    cmp r12, 2                          ; check if enough params provided
    jl .no_file

    mov rax, [rsp + 16]                 ; get the inputfile 
    mov [inputfileName], rax            ; and store in in inputfile
.itoa_done:       

; open file
    mov rax, 2                          ; open
    mov rdi, [inputfileName]            ; filename located at argv[1]
    mov rsi, 0                          ; O_RDONLY
    syscall

    mov [inputfile], rax                ; store FD
    
    cmp rax, 0                          ; failed to open file
    jle .open_failed                    ; print error message. stops program

; TODO don't fill the buffer if it already contains data. 
; Also don't try to fill with data when EOF has already been reached.
.fill_buffer:
    mov rax, 0                          ; read from
    mov rdi, [inputfile],               ; FD
    mov rsi, buffer                     ; into buffer
    add rsi, [bufferSize]               ; skipping already read bytes
    mov rdx, 512                        ; with max size 512
    sub rdx, [bufferSize]               ; minus what is already filled
    syscall

    cmp rax, 0                          ; check for errors
    jl .read_failed                     ; read failed
    add [bufferSize], rax               ; increase filled buffer size by the amount of bytes read

    cmp qword[bufferSize], 0            ; check if EOF. buffer contained no data and nothing was read
    je .process_numbers                 ; start processing

.find_number:
; find index of newline
    mov rax, 0                          ; set index to zero
    
 .find_newline:   
    cmp byte [buffer + rax], 10         ; check if newline
    je .newline_found                   ; newline found
    inc rax

    cmp rax, [bufferSize]               ; check if end of buffer 
    je .process_numbers                 ; no newline found, must have reached end of file

    jmp .find_newline                   ; no newline found yet. try the rest of the buffer

.newline_found:
    sub [bufferSize], rax               ; minus what we've read already
    sub qword [bufferSize], 1           ; off by one
    mov r15, rax                        ; preserve length of string read

    mov rdi, rax                        ; length of the string
    mov rax, buffer                     ; string in the buffer
    call atoi                           ; convert to int

    cmp rax, 0                          ; check for error
    jle .atoi_failed                    ; failed
    
    push rax                            ; push to stack
    inc r14                             ; increase r14, which holds the amount of numbers parsed

.shift_buffer:
    mov rsi, r15                        ; calculate source address starting with the length of the string
    add rsi, buffer                     ; add the buffer address
    inc rsi                             ; minus one to skip newline
    mov rdi, buffer                     ; target address
    mov rcx, 512                        ; how many bytes to copy
    sub rcx, r15                        ; minus the data already read
    sub rcx, 1                          ; offset by two to account for off-by one
    rep movsb                           ; keep moving data

    jmp .fill_buffer                    ; this number is done. Fill the buffer and try it all again.


; rax - upper bounds
; rcx - a index
; rdx - a value
; rsi - count
; rdi - previous number
.process_numbers:
    xor rsi, rsi
    mov rax, 8                          ; start calculating the start of the stack. Each entry is 64 bit
    mul r14                             ; there are r14 entries
    add rax, rsp                        ; rax is now the upper bounds of the stack

    mov rcx, rsp                        ; start with the lower bound of the stack

    mov rdi, [rcx]                      ; set rdi to first number
    add rcx, 8                          ; increment rdi by 64 bits. Skipping the first number

.get_number_a:
    mov rdx, [rcx]                      ; get current number

    cmp rdi, rdx                        ; compare to previous number
    jle .increment_number               ; don't increment counter if less or equal
    inc rsi
    
.increment_number:
    mov rdi, rdx                        ;
    add rcx, 8                          ; increment with 64 bits
    cmp rcx, rax                        ; check if upperbound met
    jne .get_number_a                   ; we are not done yet. try again

.processing_done:
    mov rax, rsi                        ; store answer in rax value

.part1_answer:
    mov r15, rax
    
    call intlen                         ; calculate length of string when displayed on screen

    mov r14, rax                        ; save strlen

    mov rax, 1                          ; start allocating space for the string
    mul r14                             ; by using strlen bytes
    sub rsp, rax                        ; move stack pointer, reserving the bytes

    mov rax, r15
    mov rdi, rsp                        ; the location of memory
    call itoa                           ; fill buffer with solution

.print:
    mov rax, 1                          ; write
    mov rdi, 2                          ; to stdout
    mov rsi, rsp                        ; answer
    mov rdx, r14                        ; the length of the msg
    syscall

    jmp .ok


; exit conditions and error messages below this point
.no_file:
    mov rax, 1                          ; write
    mov rdi, 2                          ; to stderr
    mov rsi, no_file_provided           ; no_file_provided
    mov rdx, no_file_provided_len       ; the length of the msg
    syscall

    jmp .fail

.open_failed:
    mov rax, 1                          ; write
    mov rdi, 2                          ; to stderr
    mov rsi, open_failed                ; open_failed
    mov rdx, open_failed_len            ; the length of the msg
    syscall

    jmp .fail

.read_failed:
    mov rax, 1                          ; write
    mov rdi, 2                          ; to stderr
    mov rsi, read_failed                ; open_failed
    mov rdx, read_failed_len            ; the length of the msg
    syscall

    jmp .fail

.find_readline_failed:
    mov rax, 1                          ; write
    mov rdi, 2                          ; to stderr
    mov rsi, no_newline_found           ; open_failed
    mov rdx, no_newline_found_len       ; the length of the msg
    syscall

    jmp .fail

.atoi_failed:
    mov rax, 1                          ; write
    mov rdi, 2                          ; to stderr
    mov rsi, atoi_failed                ; atoi_failed
    mov rdx, atoi_failed_len            ; the length of the msg
    syscall

    jmp .fail

.done:
.ok:
    mov rdi, 0
    jmp .exit

.fail:
    mov rdi, 1

; exit. Assumes rdi contains the exitcode
.exit:
    mov rax, 60            ; exit
    syscall
