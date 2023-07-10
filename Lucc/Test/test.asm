
.data

.code

main proc 
push rbp
mov rbp, rsp
sub rsp, 16
mov	qword ptr [rbp-8], 4
mov	eax, 40
add	eax, 1
jmp main_end
main_end:
add rsp, 16
pop rbp
ret
main endp

end
