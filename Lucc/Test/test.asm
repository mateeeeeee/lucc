extern ExitProcess: proc
extern puts : proc
extern printf : proc

.const

.data?

.data

.code

main proc 
push rbp
mov rbp, rsp
sub	rsp, 16
mov	dword ptr [rbp-20], 5
lea	rbx, qword ptr [rbp-20]
mov	qword ptr [rbp-16], rbx
mov	r10, qword ptr [rbp-16]
movsxd	rbx, r10d
mov	dword ptr [rbp-4], ebx
mov	eax, dword ptr [rbp-4]
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
