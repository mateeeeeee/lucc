public i
public arr
public p

.data
i	dword ?
arr	dword 10 dup (?)
p	qword ?

.code

main proc 
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
mov	r11, qword ptr p
mov	[r11], r10d
mov	r10d, 200
mov	r11, qword ptr p
mov	r8d, 1
imul r8, r8, 8
add	r11, r8
mov	[r11], r10d
lea	r10, i
mov	qword ptr p, r10
mov	r10d, 300
mov	r11, qword ptr p
mov	[r11], r10d
mov	r10d, dword ptr i
mov	rcx, offset arr
mov	edx, 1
imul rdx, rdx, 8
add	rcx, rdx
mov	r11, [rcx]
mov	rcx, offset arr
mov	edi, 0
imul rdi, rdi, 8
add	rcx, rdi
mov	rax, [rcx]
add	eax, r11d
add	eax, r10d
jmp main_end
main_end:
ret
main endp

end
