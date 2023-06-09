public i

.data
i	dword ?

.code

f proc 
mov	[rbp-4], ecx
mov	[rbp-8], edx
add	eax, r10d
jmp f_end
f_end:
ret
f endp

main proc 
mov	ecx, 3
mov	edx, 4
call f
mov	dword ptr i, eax
mov	eax, dword ptr i
jmp main_end
main_end:
ret
main endp

end
