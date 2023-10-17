extern ExitProcess: proc
public t

.const

.data?

.data
align 4
t	word 5

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

main proc 
push rbp
mov rbp, rsp
sub	rsp, 16
mov	dword ptr [rbp-4], 12
mov	r12d, dword ptr [rbp-4]
mov	eax, r12d
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
