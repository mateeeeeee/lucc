public k
public a
public arr

.data
k	qword ?
a	qword ?
arr	qword 10 dup (?)

.code

f proc 
mov	rax, 10
jmp f_end
f_end:
ret
f endp

main proc 
call f
mov	dword ptr k, eax
main_end:
ret
main endp

end
