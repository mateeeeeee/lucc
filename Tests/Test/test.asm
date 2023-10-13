extern ExitProcess: proc

.const

.data?

.data

.code

main proc 
push rbp
mov rbp, rsp
mov	r10d, 12
add	r10d, 34
sub	r10d, 5
mov	ebx, r10d
mov	eax, ebx
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
