
.data

.code

main proc 
push rbp
mov rbp, rsp
sub rsp, 16
mov	dword ptr [rbp-4], 5
mov	dword ptr [rbp-8], 7
mov	r10d, dword ptr [rbp-8]
mov	eax, dword ptr [rbp-4]
add	eax, r10d
jmp main_end
main_end:
add rsp, 16
pop rbp
ret
main endp

end
