
.data

.code

main proc 
mov	eax, 1
neg	eax
sar eax, 1
jmp main_end
main_end:
ret
main endp

end