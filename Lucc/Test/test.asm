extern ExitProcess: proc
public t

.const

.data?

.data
align 4
t	word 5

.code

main proc 
push rbp
mov rbp, rsp
sub	rsp, 16
mov	dword ptr [rbp-4], 12
mov	ebx, dword ptr [rbp-4]
mov	eax, ebx
jmp main_end
xor	rax, rax
main_end:
add	rsp, 16
pop rbp
mov	rcx, rax
sub	rsp, 24
call ExitProcess
ret
main endp

end
