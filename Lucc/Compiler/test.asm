public k
public l
public main

.data
k	dword ?
l	dword ?

.code

main proc
mov	k, 2
mov	l, 0
L_start1: 
mov	r10d, k
cmp	r10d, 0
setg	r10b
cmp	r10d, 0
je	L_end1
dec	k
inc	l
jmp	L_start1
L_end1: 
mov	eax, l
neg	eax
ret
main endp

end
