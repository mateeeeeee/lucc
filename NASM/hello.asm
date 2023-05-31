includelib msvcrt.lib
includelib kernel32.lib
includelib legacy_stdio_definitions.lib
includelib legacy_stdio_wide_specifiers.lib
includelib libucrt.lib

public number

extern printf		: proc

.data

number  dword 3 
msg		db "Hello world!", 0Dh, 0Ah, 0


.code

hello proc
mov eax, number
inc eax
mov number, eax
ret
hello endp

print proc
sub		 rsp, 40
lea      rcx, msg
call     printf
print endp

end
