
.data

.code

f proc 
push rbp
mov rbp, rsp
sub rsp, 16
mov	dword ptr [rbp-4], ecx
mov	dword ptr [rbp-8], edx
mov	r10d, dword ptr [rbp-4]
mov	eax, dword ptr [rbp-8]
sub	eax, r10d
jmp f_end
f_end:
add rsp, 16
pop rbp
ret
f endp

main proc 
push rbp
mov rbp, rsp
sub rsp, 32
mov	r10d, 0
mov	dword ptr [rbp-20], r10d
L_start1: 
mov	r11d, dword ptr [rbp-20]
cmp	r11d, 16
setl r10b
cmp	r10b, 0
je	L_end1
mov	ecx, dword ptr [rbp-20]
mov	edx, dword ptr [rbp-20]
add	edx, 1
call f
mov	r8d, eax
lea	r9, [rbp-16]
mov	edi, 0
imul rdi, rdi, 8
add	r9, rdi
mov	r11, qword ptr [r9]
add	r11d, r8d
lea	r8, [rbp-16]
mov	r9d, 0
imul r9, r9, 8
add	r8, r9
mov	dword ptr [r8], r11d
inc	dword ptr [rbp-20]
jmp	L_start1
L_end1: 
lea	r10, [rbp-16]
mov	r11d, 0
imul r11, r11, 8
add	r10, r11
mov	rax, qword ptr [r10]
jmp main_end
main_end:
add rsp, 32
pop rbp
ret
main endp

end
