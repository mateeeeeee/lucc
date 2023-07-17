extern ExitProcess: proc
extern puts : proc
extern printf : proc

.const

.data?

.data

.code

f proc 
push rbp
mov rbp, rsp
mov	word ptr [rbp-2], cx
mov	dword ptr [rbp-8], edx
mov	bx, word ptr [rbp-2]
mov	eax, dword ptr [rbp-8]
sub	eax, ebx
jmp f_end
f_end:
pop rbp
ret
f endp

main proc 
push rbp
mov rbp, rsp
sub	rsp, 16
mov	dword ptr [rbp-8], 5
mov	ebx, 5
neg	ebx
mov	word ptr [rbp-2], bx
sub	rsp, 32
mov	cx, word ptr [rbp-2]
mov	edx, dword ptr [rbp-8]
call f
add	rsp, 32
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
