
.const

.data?

.data

.code

main proc 
push rbp
mov rbp, rsp
sub	rsp, 32
mov	dword ptr [rbp-4], 0
L_start_0: 
mov	r10d, dword ptr [rbp-4]
cmp	r10d, 10
setl bl
movzx	rbx, bl
cmp	bl, 0
je L_end_0
mov	r11d, dword ptr [rbp-4]
cmp	r11d, 3
sete r10b
movzx	r10, r10b
cmp	r10b, 0
je L_else_1
jmp L_end_0
jmp L_end_1
L_else_1: 
L_end_1: 
L_iter_0: 
inc	dword ptr [rbp-4]
jmp L_start_0
L_end_0: 
mov	eax, dword ptr [rbp-4]
jmp main_end
xor rax, rax
main_end:
add	rsp, 32
pop rbp
ret
main endp

end
