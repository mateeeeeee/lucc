extern ExitProcess: proc

.const

.data?

.data

.code

f proc 
push rbp
mov rbp, rsp
mov	dword ptr [rbp+72], ecx
mov	dword ptr [rbp+80], edx
mov	dword ptr [rbp+88], r8d
mov	dword ptr [rbp+96], r9d
mov	r10d, dword ptr [rbp+64]
mov	r11d, dword ptr [rbp+56]
mov	r12d, dword ptr [rbp+48]
mov	r13d, dword ptr [rbp+40]
mov	r14d, dword ptr [rbp+32]
mov	r15d, dword ptr [rbp+24]
push	rbx
mov	ebx, dword ptr [rbp+16]
push	r10
mov	r10d, dword ptr [rbp+96]
push	r11
mov	r11d, dword ptr [rbp+88]
push	r12
mov	r12d, dword ptr [rbp+80]
push	r13
mov	r13d, dword ptr [rbp+72]
add	r12d, r13d
pop	r13
add	r11d, r12d
pop	r12
add	r10d, r11d
pop	r11
add	ebx, r10d
pop	r10
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
sub	rsp, 64
mov	ecx, 1
mov	edx, 2
mov	r8d, 3
mov	r9d, 4
mov	r11d, 11
push	r11
mov	r11d, 10
push	r11
mov	r11d, 9
push	r11
mov	r11d, 8
push	r11
mov	r11d, 7
push	r11
mov	r11d, 6
push	r11
mov	r11d, 5
push	r11
call f
mov	r10d, eax
add	rsp, 72
mov	dword ptr [rbp-4], r10d
mov	r10d, dword ptr [rbp-4]
mov	eax, r10d
jmp main_end
xor	rax, rax
main_end:
add	rsp, 64
pop rbp
mov	rcx, rax
sub	rsp, 32
call ExitProcess
ret
main endp

end
