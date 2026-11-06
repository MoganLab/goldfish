;; %resolve-utf8-bytes
;; 把 UTF-8 字节序列解码为字符串，非法序列返回 #f。
;;
;; 语法
;; ----
;; (%resolve-utf8-bytes byte-list)
;;
;; 参数
;; ----
;; byte-list : list?
;; UTF-8 字节数字列表。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 解码后的字符串；字节序列非法时返回 #f。

(import (scheme base) (liii check) (liii tui terminal poll))

(check-set-mode! 'report-failed)

;; UTF-8 多字节解析
(check (%resolve-utf8-bytes '(228 189 160)) => "你")
(check (%resolve-utf8-bytes '(229 165 189)) => "好")
(check (%resolve-utf8-bytes '(240 159 154 128)) => "🚀")

;; 空列表与非法字节
(check (%resolve-utf8-bytes '()) => #f)
(check (%resolve-utf8-bytes '(255 255)) => #f)

(check-report)
