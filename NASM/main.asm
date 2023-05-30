public k
public l
public main

.data
k	dword ?
l	dword ?

.code

main proc
mov	l, 0
mov	k, 2
L_start1: 
mov	r11d, k
mov	r10d, l
cmp	r10d, r11d
setl	r10b
cmp	r10d, 0
je	L_end1
inc	k
inc	l
jmp	L_start1
L_end1: 
mov	eax, k
ret
main endp

end