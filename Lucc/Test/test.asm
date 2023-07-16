
.const

.data?

.data

.code

main proc 
push rbp
mov rbp, rsp
sub	rsp, 16
mov	ebx, 55
lea	r10, qword ptr [rbp-16]
mov	r11d, 0
imul	r11, r11, 4
add	r10, r11
mov	dword ptr [r10], ebx
mov	ebx, 67
lea	r10, qword ptr [rbp-16]
mov	r11d, 1
imul	r11, r11, 4
add	r10, r11
mov	dword ptr [r10], ebx
lea	rbx, qword ptr [rbp-16]
mov	r10d, 1
imul	r10, r10, 4
add	rbx, r10
mov	qword ptr [rbp-8], rbx
mov	rbx, qword ptr [rbp-8]
mov	rax, qword ptr [rbx]
jmp main_end
xor rax, rax
main_end:
add	rsp, 16
pop rbp
ret
main endp

end
