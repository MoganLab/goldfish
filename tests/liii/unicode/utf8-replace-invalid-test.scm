(import (liii check) (liii unicode) (liii base))


(check-set-mode! 'report-failed)


;; utf8-replace-invalid
;; 把字符串中的非法 UTF-8 字节序列替换为 U+FFFD（替换字符）。
;;
;; 语法
;; ----
;; (utf8-replace-invalid str)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串，内部可能包含非法 UTF-8 字节（子进程输出、二进制数据、
;; 被截断的多字节序列等）。
;;
;; 返回值
;; ----
;; string
;; 合法 UTF-8 字节原样保留；非法字节序列按 WHATWG Encoding 规范替换为
;; U+FFFD（每个最大非法子序列一个替换字符）。输入已是合法 UTF-8 时
;; 直接返回原串。
;;
;; 错误处理
;; ----
;; type-error 当参数不是字符串时。


(define (u+fffd)
  (utf8->string #u8(239 191 189))
) ;define


;; 合法 UTF-8 不受影响
(check (utf8-replace-invalid "") => "")
(check (utf8-replace-invalid "hello world") => "hello world")
(check (utf8-replace-invalid "你好，世界") => "你好，世界")
(check (utf8-replace-invalid "a\tb\nc") => "a\tb\nc")

;; 四字节 UTF-8（emoji）
(check (utf8-replace-invalid (utf8->string #u8(240 159 152 128)))
  =>
  (utf8->string #u8(240 159 152 128))
) ;check

;; 被截断的多字节序列（“好”只剩 e5 a5）：按最大非法子序列替换为一个 U+FFFD
(check (utf8-replace-invalid (byte-vector->string #u8(229 165))) => (u+fffd))
(check (utf8-replace-invalid (string-append "你好" (byte-vector->string #u8(229 165)) "...")
       ) ;utf8-replace-invalid
  =>
  (string-append "你好" (u+fffd) "...")
) ;check

;; 孤立续字节（0x80）：非法首字节，各替换为一个 U+FFFD
(check (utf8-replace-invalid (byte-vector->string #u8(128))) => (u+fffd))
(check (utf8-replace-invalid (string-append "a" (byte-vector->string #u8(128)) "b"))
  =>
  (string-append "a" (u+fffd) "b")
) ;check

;; 非法首字节 0xFF
(check (utf8-replace-invalid (byte-vector->string #u8(255))) => (u+fffd))

;; 多个非法字节各替换为一个 U+FFFD
(check (utf8-replace-invalid (byte-vector->string #u8(254 128 129)))
  =>
  (string-append (u+fffd) (u+fffd) (u+fffd))
) ;check

;; 过长编码（overlong，0xC0 0x80）：非法序列
(check (utf8-replace-invalid (byte-vector->string #u8(192 128)))
  =>
  (string-append (u+fffd) (u+fffd))
) ;check

;; 合法序列后跟非法字节
(check (utf8-replace-invalid (string-append "ok" (byte-vector->string #u8(255))))
  =>
  (string-append "ok" (u+fffd))
) ;check

;; 类型错误检查
(check-catch 'type-error (utf8-replace-invalid 123))
(check-catch 'type-error (utf8-replace-invalid 'symbol))
(check-catch 'type-error (utf8-replace-invalid #f))
(check-catch 'type-error (utf8-replace-invalid #u8(1 2 3)))

(check-report)
