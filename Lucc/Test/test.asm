
.data

.code

main proc 
push rbp
mov rbp, rsp
sub rsp, 16
mov	dword ptr [rbp-4], 14
mov	r11d, dword ptr [rbp-4]
cmp	r11d, 10
setg r10b
movzx	r10, r10b
cmp	r10b, 0
je	L_false1
mov	eax, dword ptr [rbp-4]
sub	eax, 10
jmp	L_end1
L_false1: 
mov	eax, dword ptr [rbp-4]
sub	eax, 5
L_end1: 
jmp main_end
main_end:
add rsp, 16
pop rbp
ret
main endp

end
