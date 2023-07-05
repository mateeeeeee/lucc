
.data

.code

main proc 
push rbp
mov rbp, rsp
sub rsp, 16
mov	r10d, 3
mov	dword ptr [rbp-4], r10d
lea	rax, [rbp-4]
mov	r11d, 2
imul r11, r11, 8
add	rax, r11
lea	r10, [rbp-4]
imul r10, r10, 8
sub	rax, r10
add	eax, 3
jmp main_end
main_end:
add rsp, 16
pop rbp
ret
main endp

end
