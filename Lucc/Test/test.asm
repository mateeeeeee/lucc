
.const

.data?

.data

.code

f proc 
push rbp
mov rbp, rsp
mov	dword ptr [rbp-4], ecx
mov	dword ptr [rbp-8], edx
mov	dword ptr [rbp-12], r8d
mov	dword ptr [rbp-16], r9d
sub	rsp, 32
mov	ebx, dword ptr [rbp-16]
mov	r10d, dword ptr [rbp-12]
mov	r11d, dword ptr [rbp-8]
mov	eax, dword ptr [rbp-4]
add	eax, r11d
add	eax, r10d
add	eax, ebx
jmp f_end
f_end:
add	rsp, 32
pop rbp
ret
f endp

main proc 
push rbp
mov rbp, rsp
sub	rsp, 32
mov	ecx, 1
mov	edx, 2
mov	r8d, 3
mov	r9d, 4
call f
jmp main_end
xor rax, rax
main_end:
add	rsp, 32
pop rbp
ret
main endp

end
