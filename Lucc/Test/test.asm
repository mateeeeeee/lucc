
.data

.code

main proc 
mov	r10d, 0
cmp	r10d, 1
sete al
jmp main_end
main_end:
ret
main endp

end
