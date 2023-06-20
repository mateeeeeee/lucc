extern abs : proc

.data

.code

main proc 
push rbp
mov rbp, rsp
sub rsp, 16
mov	ecx, 5
neg	ecx
call abs
mov	r10d, eax
mov	dword ptr [rbp-4], r10d
mov	eax, dword ptr [rbp-4]
jmp main_end
main_end:
add rsp, 16
pop rbp
ret
main endp

end
