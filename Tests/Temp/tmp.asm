extern ExitProcess: proc

.const

.data?

.data

.code

main proc 
push rbp
mov rbp, rsp
mov	r10d, 6
imul	r10d, r10d, 7
mov	r11d, 5
add	r10d, r11d
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
