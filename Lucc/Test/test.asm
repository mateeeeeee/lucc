
.data

.code

main proc 
push rbp
mov rbp, rsp
sub rsp, 16
mov	dword ptr [rbp-4], 0
jmp	i
g: 
inc	dword ptr [rbp-4]
h: 
inc	dword ptr [rbp-4]
i: 
inc	dword ptr [rbp-4]
mov	eax, dword ptr [rbp-4]
jmp main_end
main_end:
add rsp, 16
pop rbp
ret
main endp

end
