public k
public main

.data
k	dword ?

.code

main proc
mov	r9, 3
mov	r8, 5
add	r8, r9
mov	k, r8
ret
main endp

end
