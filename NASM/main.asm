public main

.data
k	dword ?

.code

main proc
mov	r8d, 2
cmp	r8d, 4
setg	r8b
cmp	r8d, 0
je	L_else1
mov	r9d, 5
add	r9d, 3
mov	k, r9d
jmp	L_end1
L_else1: 
mov	r9d, 3
add	r9d, 3
mov	k, r9d
L_end1:  
mov eax, r9d
ret
main endp

end