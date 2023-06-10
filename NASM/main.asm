
.data

.code

f proc 
push rbp
mov rbp, rsp
mov	[rbp-4], ecx
mov	[rbp-8], edx
mov	r10d, [rbp-8]
mov	eax, [rbp-4]
add	eax, r10d
jmp f_end
f_end:
pop rbp
ret
f endp

main proc 
push rbp
mov rbp, rsp
mov	ecx, 3
mov	edx, 4
call f
mov	[rbp-4], eax
mov	eax, [rbp-4]
jmp main_end
main_end:
pop rbp
ret
main endp

end
