(import (liii check) (liii unicode) (liii base))

;; utf8-replace-invalid
;; 把字符串中的非法 UTF-8 字节序列替换为 U+FFFD（替换字符）。
;;
;; 语法
;; ----
;; (utf8-replace-invalid str)
;;
;; 参数
;; ----
;; str : string?
;; 输入字符串，内部可能包含非法 UTF-8 字节（子进程输出、二进制数据、
;; 被截断的多字节序列等）。
;;
;; 返回值
;; ----
;; string
;; 非法字节序列被替换为 U+FFFD 后的字符串。
;;
;; 注意
;; ----
;; 替换规则与 WHATWG Encoding 规范（及 JS TextDecoder 容错行为）一致：
;; 每个最大非法子序列替换为一个 U+FFFD；孤立续字节、过长编码（overlong）、
;; 代理区（CESU-8）与超 U+10FFFF 的序列均视为非法，越界的第二字节按
;; “退回”语义作为新首字节重新处理。
;; 输入已是合法 UTF-8 时直接返回原串对象，不重新分配。
;;
;; 错误处理
;; ----
;; type-error 当参数不是字符串时。

(check-set-mode! 'report-failed)

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

;; 过长编码（overlong，E0 后第二字节须为 A0-BF）：按 WHATWG 语义，
;; 越界的第二字节"退回"重新处理，因此 E0、80、80 各产生一个 U+FFFD
(check (utf8-replace-invalid (byte-vector->string #u8(224 128 128)))
  =>
  (string-append (u+fffd) (u+fffd) (u+fffd))
) ;check

;; 过长编码边界：E0 9F 80（9F < A0）
(check (utf8-replace-invalid (byte-vector->string #u8(224 159 128)))
  =>
  (string-append (u+fffd) (u+fffd) (u+fffd))
) ;check

;; 代理区（ED 后第二字节须为 80-9F，拒绝 CESU-8 编码的 D800-DFFF）
(check (utf8-replace-invalid (byte-vector->string #u8(237 160 128)))
  =>
  (string-append (u+fffd) (u+fffd) (u+fffd))
) ;check

;; 过长 4 字节（F0 后第二字节须为 90-BF）
(check (utf8-replace-invalid (byte-vector->string #u8(240 143 191 191)))
  =>
  (string-append (u+fffd) (u+fffd) (u+fffd) (u+fffd))
) ;check

;; 超 U+10FFFF（F4 后第二字节须为 80-8F）
(check (utf8-replace-invalid (byte-vector->string #u8(244 144 128 128)))
  =>
  (string-append (u+fffd) (u+fffd) (u+fffd) (u+fffd))
) ;check

;; 越界序列混在正常文本中
(check (utf8-replace-invalid (string-append "a" (byte-vector->string #u8(237 160 128)) "b"))
  =>
  (string-append "a" (u+fffd) (u+fffd) (u+fffd) "b")
) ;check

;; 第二字节合法但第三字节非法：E0 A0 是最大非法子序列，41 "退回"
(check (utf8-replace-invalid (byte-vector->string #u8(224 160 65)))
 =>
  (string-append (u+fffd) "A")
) ;check

;; 合法边界不受影响：U+0800 / U+D7FF / U+10000 / U+10FFFF
(check (utf8-replace-invalid (byte-vector->string #u8(224 160 128)))
  => (byte-vector->string #u8(224 160 128)))
(check (utf8-replace-invalid (byte-vector->string #u8(237 159 191)))
  => (byte-vector->string #u8(237 159 191)))
(check (utf8-replace-invalid (byte-vector->string #u8(240 144 128 128)))
  => (byte-vector->string #u8(240 144 128 128)))
(check (utf8-replace-invalid (byte-vector->string #u8(244 143 191 191)))
  => (byte-vector->string #u8(244 143 191 191)))

;; 类型错误检查
(check-catch 'type-error (utf8-replace-invalid 123))
(check-catch 'type-error (utf8-replace-invalid 'symbol))
(check-catch 'type-error (utf8-replace-invalid #f))
(check-catch 'type-error (utf8-replace-invalid #u8(1 2 3)))

(check-report)
