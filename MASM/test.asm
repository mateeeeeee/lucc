
includelib <kernel32.lib>


extern  ExitProcess: PROC
extern puts : PROC
.const

.data?


.data
message BYTE "Another message! Longer this time!", 0

.code

main proc
lea rcx, message
call puts

xor rax, rax

mov rcx, rax
call ExitProcess
ret
main endp

end
