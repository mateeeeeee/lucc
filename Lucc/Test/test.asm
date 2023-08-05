extern ExitProcess: proc
extern printf : proc

.const

.data?

.data

.code

main proc 
push rbp
mov rbp, rsp
sub	rsp, 16
mov	dword ptr [rbp-12], 5
mov	r10d, 7
neg	r10d
mov	bx, r10w
mov	word ptr [rbp-8], bx
mov	dword ptr [rbp-4], 10
mov	r10d, dword ptr [rbp-12]
mov	r11w, word ptr [rbp-8]
mov	r12d, dword ptr [rbp-4]
add	r11d, r12d
add	r10d, r11d
mov	ebx, r10d
mov	eax, ebx
jmp main_end
xor	rax, rax
main_end:
add	rsp, 16
pop rbp
mov	rcx, rax
sub	rsp, 32
call ExitProcess
ret
main endp

end
