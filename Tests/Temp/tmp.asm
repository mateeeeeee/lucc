
.data

.code

main proc 
push rbp
mov rbp, rsp
sub rsp, 16
mov	dword ptr [rbp-4], 5
mov	eax, dword ptr [rbp-4]
jmp main_end
main_end:
add rsp, 16
pop rbp
ret
main endp

end
