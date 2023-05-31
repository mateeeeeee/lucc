public l
public main

.data
k	dword ?
l	dword ?

.code

f proc
f_end:
ret
f endp

main proc
mov	k, 2
mov	eax, 5
jmp main_end
mov	l, 3
call f
mov	r10d, k
mov	eax, l
sub	eax, r10d
sub	eax, 1
jmp main_end
main_end:
ret
main endp

end
