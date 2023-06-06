public k
public a
public arr

.data
k	dword ?
a	qword ?
arr	dword 10 dup (?)

.code

f proc
mov	eax, 10
jmp f_end
f_end:
ret
f endp

main proc
call f
mov	dword ptr k, eax
mov	r10d, 12
mov	r11, offset arr
mov	r8d, 0
imul	r8, r8, 8
add	r11, r8
mov	[r11], r10d
mov	r10, offset arr
mov	r11d, 0
imul	r11, r11, 8
add	r10, r11
mov	rax, [r10]
jmp main_end
main_end:
ret
main endp

end
