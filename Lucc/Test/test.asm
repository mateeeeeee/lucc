extern ExitProcess: proc
public a

.const
align 4
a	word 5

.data?

.data

.code

main proc 
push rbp
mov rbp, rsp
sub	rsp, 16
mov	dword ptr [rbp-4], 9
mov	dword ptr [rbp-4], 10
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
