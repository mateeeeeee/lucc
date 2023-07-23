extern ExitProcess: proc

.const

.data?

.data

.code

main proc 
push rbp
mov rbp, rsp
sub	rsp, 16
sub	rsp, 32
mov	ecx, 1
mov	edx, 2
call f
add	rsp, 32
mov	dword ptr [rbp-4], eax
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

f proc 
push rbp
mov rbp, rsp
mov	dword ptr [rbp-4], ecx
mov	dword ptr [rbp-8], edx
mov	r10d, dword ptr [rbp-8]
mov	ebx, dword ptr [rbp-4]
add	ebx, r10d
mov	eax, ebx
jmp f_end
f_end:
pop rbp
ret
f endp

end
