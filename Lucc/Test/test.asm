extern ExitProcess: proc
extern puts : proc
extern printf : proc

.const
L_str_0 byte "Mate",0

.data?

.data

.code

main proc 
push rbp
mov rbp, rsp
sub	rsp, 16
mov	rbx, offset L_str_0
mov	qword ptr [rbp-8], rbx
sub	rsp, 32
mov	rcx, qword ptr [rbp-8]
call printf
add	rsp, 32
mov	eax, 0
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
