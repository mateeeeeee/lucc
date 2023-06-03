public a
extern f : proc

.data
k	qword ?
a	qword ?

.code

main proc 
call f
mov	k, rax
lea	r10, k
mov	a, r10
mov	r10, a
mov	rax, [r10]
add	rax, 1
jmp main_end
main_end:
ret
main endp

end
