extern ExitProcess: proc
extern puts : proc
extern printf : proc
public c

.const

.data?
align 4
c	word ?

.data

.code

main proc 
push rbp
mov rbp, rsp
sub	rsp, 16
mov	dword ptr c, 10
mov	ebx, dword ptr c
mov	dword ptr [rbp-4], ebx
mov	eax, 4
jmp main_end
xor	rax, rax
main_end:
add	rsp, 16
pop rbp
mov	rcx, rax
sub	rsp, 32
call ExitProcess
ret
main endp

end
