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
