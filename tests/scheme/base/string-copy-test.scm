(import (liii check))
(import (scheme base))

(check-set-mode! 'report-failed)

;; string-copy
;; 创建字符串的副本，可以选择复制部分子串。
;; 注意：本实现使用字节索引而非字符索引。
;;
;; 语法
;; ----
;; (string-copy str)
;; (string-copy str start)
;; (string-copy str start end)
;;
;; 参数
;; ----
;; str : string?
;; 源字符串。
;;
;; start : integer? 可选
;; 开始索引（包含，字节位置），默认为 0。
;;
;; end : integer? 可选
;; 结束索引（不包含，字节位置），默认为字符串长度（字节）。
;;
;; 返回值
;; ------
;; string?
;; 返回一个新的字符串副本。

;; 完整复制
(check (string-copy "hello") => "hello")
(check (string-copy "") => "")
(check (string-copy "abc") => "abc")

;; 子串复制（ASCII字符串，字节索引=字符索引）
(check (string-copy "hello" 0) => "hello")
(check (string-copy "hello" 1) => "ello")
(check (string-copy "hello" 2) => "llo")
(check (string-copy "hello" 4) => "o")
(check (string-copy "hello" 5) => "")

;; 指定范围复制
(check (string-copy "hello" 0 5) => "hello")
(check (string-copy "hello" 0 2) => "he")
(check (string-copy "hello" 1 4) => "ell")
(check (string-copy "hello" 2 3) => "l")
(check (string-copy "hello" 3 3) => "")

;; 中文字符串（UTF-8，每个字符3字节）
(check (string-copy "你好世界") => "你好世界")
;; "你好世界" - "你"=3字节, "好"=3字节, "世"=3字节, "界"=3字节
(check (string-copy "你好世界" 3) => "好世界")   ; 从第4字节开始（"好"）
(check (string-copy "你好世界" 6) => "世界")   ; 从第7字节开始（"世"）
(check (string-copy "你好世界" 0 3) => "你")   ; 前3字节（"你"）
(check (string-copy "你好世界" 3 6) => "好")   ; 第4-6字节（"好"）
(check (string-copy "你好世界" 0 6) => "你好") ; 前6字节（"你好"）

(check-report)
