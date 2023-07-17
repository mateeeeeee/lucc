extern ExitProcess: proc
extern puts : proc

.const
L_str_0 byte "abc",0

.data?

.data

.code

main proc 
push rbp
mov rbp, rsp
sub	rsp, 16
mov	rbx, offset L_str_0
mov	qword ptr [rbp-16], rbx
sub	rsp, 32
mov	rcx, qword ptr [rbp-16]
call puts
add	rsp, 32
mov	dword ptr [rbp-4], 7
mov	ebx, dword ptr [rbp-4]
mov	r10d, 3
xor	rdx, rdx
mov	eax, ebx
idiv	r10d
mov	ebx, eax
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
