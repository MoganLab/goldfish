(import (liii check)
        (liii string)
        (liii pi code utils shell))

(check-set-mode! 'report-failed)

;; sanitize-binary-output
;; 清理命令输出中会导致显示错乱或宽度计算崩溃的字符：
;; - 删除控制字符 \x00-\x08、\x0B、\x0C、\x0E-\x1F（保留 \t \n \r）
;; - 删除 U+FFF9..U+FFFB（interlinear annotation，会使宽度计算崩溃）

;; 非法 UTF-8 序列：bash 命令输出可能含截断的多字节序列（如被截断的“好”
;; 只剩 e5 a5）或二进制垃圾；与上游 JS TextDecoder 一致，替换为 U+FFFD
;; 而不是抛错（否则整个工具输出丢失，消息卡片显示 value-error）
;; （用 byte-vector->string 构造含非法字节的原始字符串）
(check (sanitize-binary-output (string-append "你好" (byte-vector->string #u8(229 165)) "..."))
  =>
  (string-append "你好" (utf8->string #u8(239 191 189)) "...")
) ;check

;; 非法首字节（0xFF 孤立字节）
(check (sanitize-binary-output (string-append "a" (byte-vector->string #u8(255)) "b"))
  =>
  (string-append "a" (utf8->string #u8(239 191 189)) "b")
) ;check

;; 多个非法字节各替换为一个 U+FFFD
(check (sanitize-binary-output (byte-vector->string #u8(254 128 129)))
  =>
  (string-append (utf8->string #u8(239 191 189))
    (utf8->string #u8(239 191 189))
    (utf8->string #u8(239 191 189))
  ) ;string-append
) ;check

;; 代理区（ED A0 80，CESU-8 编码的 D800）：按 WHATWG 替换为 3 个 U+FFFD
(check (sanitize-binary-output (byte-vector->string #u8(237 160 128)))
  =>
  (string-append (utf8->string #u8(239 191 189))
    (utf8->string #u8(239 191 189))
    (utf8->string #u8(239 191 189))
  ) ;string-append
) ;check

;; 普通文本不受影响
(check (sanitize-binary-output "hello world") => "hello world")
(check (sanitize-binary-output "你好，世界") => "你好，世界")

;; 空字符串
(check (sanitize-binary-output "") => "")

;; 保留 \t \n \r
(check (sanitize-binary-output "a\tb\nc\rd") => "a\tb\nc\rd")

;; 删除 C0 控制字符（除 \t \n \r）
;; 注：十六进制转义必须用 ; 终结，否则会贪婪吞掉后续十六进制字符
(check (sanitize-binary-output "a\x00;b") => "ab")
(check (sanitize-binary-output "a\x01;b\x02;c") => "abc")
(check (sanitize-binary-output "a\x07;b") => "ab")          ; BEL
(check (sanitize-binary-output "a\x08;b") => "ab")          ; BS
(check (sanitize-binary-output "a\x0b;b\x0c;c") => "abc")  ; VT / FF
(check (sanitize-binary-output "a\x0e;b\x1f;c") => "abc")  ; SO .. US
(check (sanitize-binary-output "a\x1b;b") => "ab")          ; ESC（裸 ESC 无 [ 或 ]）

;; 删除 U+FFF9..U+FFFB
(check (sanitize-binary-output (string-append "a" (utf8->string #u8(239 191 185)) "b")) => "ab")
(check (sanitize-binary-output (string-append "x" (utf8->string #u8(239 191 187)))) => "x")

;; 保留相邻的合法 Unicode（U+FFFD 替换字符不受影响）
(check (sanitize-binary-output (string-append "a" (utf8->string #u8(239 191 189)) "b"))
  => (string-append "a" (utf8->string #u8(239 191 189)) "b"))

;; 类型错误检查
(check-catch 'type-error (sanitize-binary-output 123))
(check-catch 'type-error (sanitize-binary-output 'sym))
(check-catch 'type-error (sanitize-binary-output #f))

(check-report)
