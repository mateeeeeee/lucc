
.data

.code

f proc 
push rbp
mov rbp, rsp
sub rsp, 16
mov	qword ptr [rbp-8], rcx
mov	r10d, 5
mov	r11, qword ptr [rbp-8]
mov	dword ptr [r11], r10d
f_end:
add rsp, 16
pop rbp
ret
f endp

main proc 
push rbp
mov rbp, rsp
sub rsp, 16
mov	dword ptr [rbp-4], 1
lea	rcx, [rbp-4]
call f
mov	eax, dword ptr [rbp-4]
jmp main_end
main_end:
add rsp, 16
pop rbp
ret
main endp

end
