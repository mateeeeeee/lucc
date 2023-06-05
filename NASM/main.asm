public k
public a
public arr

.data
k	qword ?
a	qword ?
arr	qword 1,2,3,4,5

.code

main proc 
mov rcx, qword ptr offset arr
add rcx, 16
mov rdx, [rcx]
mov rax, rdx
main_end:
ret
main endp

end


