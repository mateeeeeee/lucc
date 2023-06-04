

public k
public a

.data
k	qword 2, 17, 29
a	qword ?

.code

f proc 
mov	rax, 10
jmp f_end
f_end:
ret
f endp

main proc 
;call f
;mov	k, rax
;mov	r10, k
;mov	[k + 4], r10
lea	r10, [k + 16]
mov	rax, [r10]
jmp main_end
main_end:
ret
main endp

end

