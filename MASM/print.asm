;;includelib libucrt.lib
;;includelib kernel32.lib
;;
;;extern puts :proc
;;extern printf :proc
;;extern ExitProcess :proc
;;
;;.data
;;
;;string_title_x64_printf db "title x64 printf",0
;;string_color_0F db "color 0F",0
;;string_pause db "pause",0
;;string db "string",0
;;string_pd_newline db "%d",10,0
;;
;;.code
;;
;;main proc 
;;
;;   lea rcx, string
;;   call puts
;;
;;   lea rcx, string_pd_newline
;;   mov rdx, 3
;;   call printf
;;
;;   mov rcx, 0
;;   call ExitProcess
;;   ret
;;main endp
;;
end