
.data

.code

main proc 
push rbp
mov rbp, rsp
sub rsp, 16
mov	dword ptr [rbp-8], 0
mov	dword ptr [rbp-4], 0
L_start1: 
mov	r11d, dword ptr [rbp-8]
cmp	r11d, 10
setle r10b
movzx	r10, r10b
cmp	r10b, 0
je	L_end1
mov	r8d, dword ptr [rbp-4]
mov	r11d, dword ptr [rbp-8]
add	r11d, r8d
mov	dword ptr [rbp-4], r11d
mov	r11d, dword ptr [rbp-8]
add	r11d, 1
mov	dword ptr [rbp-8], r11d
jmp	L_start1
L_end1: 
mov	eax, dword ptr [rbp-4]
jmp main_end
main_end:
add rsp, 16
pop rbp
ret
main endp

end
