bits 64
default rel

segment .data
    fmt db "factorial is: %d", 0xd, 0xa, 0

segment .text

global factorial
factorial:
    push    rbp
    mov     rbp, rsp
    sub     rsp, 32

    test    ecx, ecx
    jz     .zero

    mov    ebx, 1       ; counter c
    mov    eax, 1       ; result

    inc    ecx

.for_loop:
    cmp    ebx, ecx
    je     .end_loop

    mul    ebx          ; multiply ebx * eax and store in eax

    inc    ebx          ; ++c
    jmp    .for_loop


.zero:
    mov    eax, 1

.end_loop:
    mov  rsp, rbp 
    pop  rbp
    ret



;global main
;extern ExitProcess
;extern _CRT_INIT
;
;extern printf
;
;main:
;    push    rbp
;    mov     rbp, rsp
;    sub     rsp, 32
;
;    call    _CRT_INIT
;
;    mov     rcx, 6
;    call    factorial
;
;   lea      rcx, [fmt]
;    mov     rdx, rax
;    call    printf
;
;    xor     rax, rax
;    call    ExitProcess