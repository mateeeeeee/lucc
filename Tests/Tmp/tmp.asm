extern ExitProcess: proc

.const

.data?

.data

.code

main proc 
push rbp
mov rbp, rsp
sub	rsp, 16
mov	dword ptr [rbp-12], 1
mov	r10d, 48
add	r10d, 2
mov	ebx, r10d
mov	dword ptr [rbp-8], ebx
mov	r10d, dword ptr [rbp-8]
mov	r11d, dword ptr [rbp-12]
add	r10d, r11d
mov	ebx, r10d
mov	dword ptr [rbp-4], ebx
mov	r10d, dword ptr [rbp-4]
imul	r10d, r10d, 2
mov	ebx, r10d
mov	eax, ebx
jmp main_end
xor	rax, rax
main_end:
add	rsp, 16
pop rbp
mov	rcx, rax
sub	rsp, 24
call ExitProcess
ret
main endp

end
