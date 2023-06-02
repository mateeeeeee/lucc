public a
extern f : proc

.data
k	dword ?
a	dword ?

.code

main proc 
call f
mov	k, eax
mov	a, r10d
add	eax, 1
jmp main_end
main_end:
ret
main endp

end
