
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
sub rsp, 16
mov	r10d, 0
mov	dword ptr [rbp-4], r10d
mov	r10d, 0
mov	dword ptr [rbp-8], r10d
L_start1: 
mov	r11d, dword ptr [rbp-8]
cmp	r11d, 10
setl r10b
cmp	r10b, 0
je	L_end1
mov	ecx, dword ptr [rbp-8]
mov	edx, dword ptr [rbp-8]
add	edx, 1
call f
mov	r8d, eax
mov	r11d, dword ptr [rbp-4]
add	r11d, r8d
mov	dword ptr [rbp-4], r11d
inc	dword ptr [rbp-8]
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
