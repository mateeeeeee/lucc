public l

.data
k	dword ?
l	dword ?

.code

f proc 
mov	eax, 5
jmp f_end
f_end:
ret
f endp

main proc 
call f
mov	k, eax
mov	l, 3
mov	r10d, k
mov	eax, l
sub	eax, r10d
sub	eax, 1
jmp main_end
main_end:
ret
main endp

end
