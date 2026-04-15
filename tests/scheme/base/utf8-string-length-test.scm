(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; utf8-string-length
;; 返回 UTF-8 字符串的字符数（不是字节数）。
;;
;; 语法
;; ----
;; (utf8-string-length str)
;;
;; 参数
;; ----
;; str : string?
;; UTF-8 编码的字符串。
;;
;; 返回值
;; ------
;; integer?
;; 返回字符串的字符数。
;; ASCII 字符串测试
(check (utf8-string-length "") => 0)
(check (utf8-string-length "hello")
  =>
  5
) ;check
(check (utf8-string-length "a") => 1)
;; 中文字符串测试
(check (utf8-string-length "你好")
  =>
  2
) ;check
(check (utf8-string-length "你好世界")
  =>
  4
) ;check
(check (utf8-string-length "中") => 1)
;; 混合字符串测试
(check (utf8-string-length "Hello世界")
  =>
  7
) ;check
(check (utf8-string-length "a中b")
  =>
  3
) ;check
;; Emoji 测试（如果支持）
;; 日文测试
(check (utf8-string-length "こんにちは")
  =>
  5
) ;check
;; 韩文测试
(check (utf8-string-length "안녕하세요")
  =>
  5
) ;check
;; 错误测试
(check-catch 'wrong-type-arg
  (utf8-string-length 123)
) ;check-catch
(check-catch 'wrong-type-arg
  (utf8-string-length 'symbol)
) ;check-catch
(check-report)