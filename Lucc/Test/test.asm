extern ExitProcess: proc

.const

.data?

.data

.code

f proc 
push rbp
mov rbp, rsp
mov	dword ptr [rbp-4], ecx
mov	dword ptr [rbp-8], edx
mov	dword ptr [rbp-12], r8d
mov	dword ptr [rbp-16], r9d
mov	r10d, dword ptr [rbp+32]
mov	r11d, dword ptr [rbp+24]
mov	r12d, dword ptr [rbp+16]
mov	r13d, dword ptr [rbp-16]
mov	r14d, dword ptr [rbp-12]
mov	r15d, dword ptr [rbp-8]
push	rbx
mov	ebx, dword ptr [rbp-4]
add	r15d, ebx
pop	rbx
add	r14d, r15d
add	r13d, r14d
add	r12d, r13d
add	r11d, r12d
add	r10d, r11d
mov	ebx, r10d
mov	eax, ebx
jmp f_end
f_end:
pop rbp
ret
f endp

main proc 
push rbp
mov rbp, rsp
sub	rsp, 16
sub	rsp, 48
mov	ecx, 1
mov	edx, 2
mov	r8d, 3
mov	r9d, 4
mov	r11d, 7
push	r11
mov	r11d, 6
push	r11
mov	r11d, 5
push	r11
call f
mov	r10d, eax
add	rsp, 56
mov	dword ptr [rbp-4], r10d
mov	r10d, dword ptr [rbp-4]
mov	eax, r10d
jmp main_end
xor	rax, rax
main_end:
add	rsp, 32
pop rbp
mov	rcx, rax
sub	rsp, 32
call ExitProcess
ret
main endp

end
