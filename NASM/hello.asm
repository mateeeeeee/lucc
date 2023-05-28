
public number

.data

number dword 3 


.code

f proc
mov eax,number
inc eax
mov number, eax
ret
f endp

end
