extern ExitProcess: proc

.const

.data?

.data

.code

main proc 
push rbp
mov rbp, rsp
sub	rsp, 16
mov	dword ptr [rbp-4], 2
mov	r10d, dword ptr [rbp-4]
add	r10d, 5
mov	ebx, r10d
mov	dword ptr [rbp-4], ebx
mov	ebx, dword ptr [rbp-4]
mov	eax, ebx
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
