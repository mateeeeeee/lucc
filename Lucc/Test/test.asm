extern ExitProcess: proc

.const

.data?

.data

.code

f proc 
push rbp
mov rbp, rsp
mov	dword ptr [rbp-4], ecx
mov	eax, 16
jmp f_end
f_end:
pop rbp
ret
f endp

main proc 
push rbp
mov rbp, rsp
sub	rsp, 16
mov	rbx, qword ptr f
mov	qword ptr [rbp-8], rbx
sub	rsp, 32
mov	ecx, 1
call pf
add	rsp, 32
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
