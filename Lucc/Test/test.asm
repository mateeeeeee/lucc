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
mov	dword ptr [rbp-8], 65535
mov	r10d, dword ptr [rbp-8]
mov	bl, r10b
mov	byte ptr [rbp-1], bl
mov	r10b, byte ptr [rbp-1]
movzx	ebx, r10b
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
