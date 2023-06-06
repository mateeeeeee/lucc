public i
public arr
public p

.data
i	dword ?
arr	dword 10 dup (?)
p	qword ?

.code

main proc 
mov	dword ptr i, 0
L_start1: 
mov	r11d, dword ptr i
cmp	r11d, 10
setl r10b
cmp	r10b, 0
je	L_end1
mov	r11d, dword ptr i
add	r11d, 10
mov	r8, offset arr
mov	r9d, dword ptr i
imul r9, r9, 8
add	r8, r9
mov	[r8], r11d
inc	i
jmp	L_start1
L_end1: 
mov	r10, offset arr
mov	qword ptr p, r10
mov	r10d, 100
mov	r11, p
mov	[r11], r10d
mov	r10, offset arr
mov	r11d, 0
imul r11, r11, 8
add	r10, r11
mov	rax, [r10]
jmp main_end
main_end:
ret
main endp

end
