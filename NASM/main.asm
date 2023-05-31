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
mov	r8d, k
mov	r11d, l
cmp	r11d, r8d
setl	r10b
cmp	r10d, 0
je	L_end1
inc	l
jmp	L_start1
L_end1: 
mov	eax, l
sub	eax, 2
ret
main endp

end
