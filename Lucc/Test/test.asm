extern ExitProcess: proc
extern puts : proc
public x
public y

.const

.data?

.data
align 16
x	qword ?
align 16
y	qword ?

.code

main proc
push rbp
mov rbp, rsp
lea	rax, qword ptr y
lea	rbx, qword ptr x
sub	rax, rbx
jmp main_end
xor	rax, rax
main_end:
pop rbp
mov	rcx, rax
sub	rsp, 32
call ExitProcess
ret
main endp

end
