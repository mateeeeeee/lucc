public k
public main

.data
k	dword ?

.code

main proc
mov	r8d, 5
mov	k, r8d
;xor eax, eax
mov eax, k
ret
main endp

end