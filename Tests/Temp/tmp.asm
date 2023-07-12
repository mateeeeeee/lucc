
.data

.code

main proc 
push rbp
mov rbp, rsp
sub rsp, 16
mov	dword ptr [rbp-4], 5
mov	r10d, dword ptr [rbp-4]
sub	r10d, 7
mov	dword ptr [rbp-4], r10d
mov	eax, dword ptr [rbp-4]
jmp main_end
main_end:
add rsp, 16
pop rbp
ret
main endp

end
