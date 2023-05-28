
public number

.data

number dword 3 


.code

hello proc
mov eax,number
inc eax
mov number, eax
ret
hello endp

end
