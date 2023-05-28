public k
public main

.data
k	dword ?

.code

main proc
mov	r10d, 2
cmp	r10d, 4
setg	r10b
cmp	r10d, 0
je	L_else1
mov	r11d, 5
add	r11d, 3
mov	k, r11d
jmp	L_end1
L_else1: 
mov	r11d, 3
add	r11d, 3
mov	k, r11d
L_end1: 
mov	eax, k
ret
main endp

end
