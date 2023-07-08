
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
cmp	r11d, 3
setl r10b
movzx	r10, r10b
cmp	r10b, 0
je	L_end1
mov	r8d, dword ptr [rbp-8]
cmp	r8d, 1
setg r11b
movzx	r11, r11b
cmp	r11b, 0
je	L_else2
jmp	L_start1
jmp	L_end2
L_else2: 
L_end2: 
inc	dword ptr [rbp-4]
inc	dword ptr [rbp-8]
jmp	L_start1
L_end1: 
mov	eax, dword ptr [rbp-8]
jmp main_end
main_end:
add rsp, 16
pop rbp
ret
main endp

end
