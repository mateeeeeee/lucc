extern ExitProcess: proc

.const

.data?

.data

.code

f proc 
push rbp
mov rbp, rsp
mov	eax, 10
jmp f_end
f_end:
pop rbp
ret
f endp

main proc 
push rbp
mov rbp, rsp
sub	rsp, 16
mov	r10d, 0
mov	rbx, r10
mov	qword ptr [rbp-8], rbx
mov	rbx, qword ptr [rbp-8]
cmp	bl, 0
je L_else_0
mov	eax, 10
jmp main_end
jmp L_end_0
L_else_0: 
mov	eax, 20
jmp main_end
L_end_0: 
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
