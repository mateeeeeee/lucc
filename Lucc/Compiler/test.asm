public k
public l
public f
public main

.data
k	dword ?
l	dword ?

.code

f proc
ret
f endp

main proc
mov	k, 2
mov	l, 3
call f
mov	r10d, k
mov	eax, l
sub	eax, r10d
sub	eax, 1
ret
main endp

end
