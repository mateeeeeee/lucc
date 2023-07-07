
.data

.code

main proc 
push rbp
mov rbp, rsp
sub rsp, 32
mov	r10d, 0
lea	r11, [rbp-20]
mov	r8d, 0
imul	r8, r8, 4
add	r11, r8
mov	dword ptr [r11], r10d
mov	r10d, 1
lea	r11, [rbp-20]
mov	r8d, 1
imul	r8, r8, 4
add	r11, r8
mov	dword ptr [r11], r10d
mov	r10d, 2
lea	r11, [rbp-20]
mov	r8d, 2
imul	r8, r8, 4
add	r11, r8
mov	dword ptr [r11], r10d
lea	r10, [rbp-20]
mov	r11d, 1
imul	r11, r11, 4
add	r10, r11
mov	qword ptr [rbp-8], r10
mov	r10, qword ptr [rbp-8]
dec	dword ptr [r10]
lea	r10, [rbp-20]
mov	r11d, 2
imul	r11, r11, 4
add	r10, r11
mov	rax, qword ptr [r10]
jmp main_end
main_end:
add rsp, 32
pop rbp
ret
main endp

end
