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
mov	k, rax
mov	[], 12
mov	r10, arr
add	r10, 0
mov	rax, [r10]
jmp main_end
main_end:
ret
main endp

end
