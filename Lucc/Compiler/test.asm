public l
extern f : proc

.data
k	dword ?
l	dword ?

.code

main proc 
call f
mov	k, eax
mov	l, 3
mov	r10d, l
mov	eax, k
sub	eax, r10d
sub	eax, 5
jmp main_end
main_end:
ret
main endp

end
