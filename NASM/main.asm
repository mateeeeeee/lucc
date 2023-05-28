public main

.data
k	dword ?

.code

main proc
cmp	r8d, 0
jz	L_else1
mov	r10d, 3
mov	r9d, 5
add	r9d, r10d
mov	k, r9d
jmp	L_end1
L_else1: 
mov	r10d, 3
mov	r9d, 3
add	r9d, r10d
mov	k, r9d
L_end1:  
mov eax, r9d
ret
main endp

end