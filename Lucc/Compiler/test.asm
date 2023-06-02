public a
extern f : proc

.data
k	dword ?
a	dword ?

.code

main proc 
call f
mov	k, eax
lea	r10, k
mov	a, r10d
mov	r10d, a
mov	eax, [r10d]
jmp main_end
main_end:
ret
main endp

end
