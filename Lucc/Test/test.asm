extern ExitProcess: proc
extern puts : proc
extern printf : proc

.const

.data?

.data

.code

main proc 
push rbp
mov rbp, rsp
sub	rsp, 16
mov	ebx, 1
neg	ebx
mov	word ptr [rbp-6], bx
mov	r10w, word ptr [rbp-6]
movsx	ebx, r10w
mov	dword ptr [rbp-4], ebx
mov	eax, dword ptr [rbp-4]
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
