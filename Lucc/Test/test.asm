extern ExitProcess: proc

.const

.data?

.data

.code

f2 proc 
push rbp
mov rbp, rsp
mov	dword ptr [rbp+16], ecx
mov	dword ptr [rbp+24], edx
mov	r10d, dword ptr [rbp+24]
mov	r11d, dword ptr [rbp+16]
add	r10d, r11d
mov	ebx, r10d
mov	eax, ebx
jmp f2_end
f2_end:
pop rbp
ret
f2 endp

f1 proc 
push rbp
mov rbp, rsp
mov	dword ptr [rbp+16], ecx
mov	r11d, dword ptr [rbp+16]
add	r11d, 1
mov	r10d, r11d
mov	eax, r10d
jmp f1_end
f1_end:
pop rbp
ret
f1 endp

f4 proc 
push rbp
mov rbp, rsp
mov	dword ptr [rbp+16], ecx
mov	dword ptr [rbp+24], edx
mov	dword ptr [rbp+32], r8d
mov	dword ptr [rbp+40], r9d
mov	r12d, dword ptr [rbp+40]
mov	r13d, dword ptr [rbp+32]
mov	r14d, dword ptr [rbp+24]
mov	r15d, dword ptr [rbp+16]
add	r14d, r15d
add	r13d, r14d
add	r12d, r13d
mov	r11d, r12d
mov	eax, r11d
jmp f4_end
f4_end:
pop rbp
ret
f4 endp

f5 proc 
push rbp
mov rbp, rsp
mov	dword ptr [rbp+24], ecx
mov	dword ptr [rbp+32], edx
mov	dword ptr [rbp+40], r8d
mov	dword ptr [rbp+48], r9d
mov	r13d, dword ptr [rbp+16]
mov	r14d, dword ptr [rbp+48]
mov	r15d, dword ptr [rbp+40]
push	rbx
mov	ebx, dword ptr [rbp+32]
push	r10
mov	r10d, dword ptr [rbp+24]
add	ebx, r10d
pop	r10
add	r15d, ebx
pop	rbx
add	r14d, r15d
add	r13d, r14d
mov	r12d, r13d
mov	eax, r12d
jmp f5_end
f5_end:
pop rbp
ret
f5 endp

main proc 
push rbp
mov rbp, rsp
sub	rsp, 32
mov	dword ptr [rbp-20], 1
mov	dword ptr [rbp-16], 2
mov	dword ptr [rbp-12], 3
mov	dword ptr [rbp-8], 4
mov	dword ptr [rbp-4], 5
sub	rsp, 48
mov	ecx, dword ptr [rbp-20]
mov	edx, dword ptr [rbp-16]
mov	r8d, dword ptr [rbp-12]
mov	r9d, dword ptr [rbp-8]
mov	r13d, dword ptr [rbp-4]
push	r13
call f5
add	rsp, 56
jmp main_end
xor	rax, rax
main_end:
add	rsp, 32
pop rbp
mov	rcx, rax
sub	rsp, 24
call ExitProcess
ret
main endp

end
