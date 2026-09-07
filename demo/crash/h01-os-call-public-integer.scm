;; (liii os) 的 os-call 公开 API 直接透传给 g_os-call，
;; 不检查参数类型：整数被当作命令字符串指针 => SIGSEGV
(import (liii os))
(os-call 42)
