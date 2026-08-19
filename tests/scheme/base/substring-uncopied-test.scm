(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; substring-uncopied
;; 返回与原字符串共享存储的子串（不可变，供短暂使用）。
;;
;; 语法
;; ----
;; (substring-uncopied str start)
;; (substring-uncopied str start end)
;;
;; 参数
;; ----
;; str : string?
;; 源字符串。
;;
;; start : integer?
;; 开始索引（包含）。
;;
;; end : integer? 可选
;; 结束索引（不包含），默认为字符串长度。
;;
;; 返回值
;; ------
;; string?
;; 返回共享存储的子串。

;; 基础：省略 end
(check (substring-uncopied "01234" 1) => "1234")
;; 基础：指定 end
(check (substring-uncopied "01234" 1 3) => "12")
;; 边界：start 为 0
(check (substring-uncopied "01234" 0) => "01234")
;; 边界：start 等于 end，得到空串
(check (substring-uncopied "01234" 2 2) => "")
;; 边界：end 为字符串长度
(check (substring-uncopied "01234" 0 5) => "01234")
;; 空字符串
(check (substring-uncopied "" 0) => "")

;; 错误：start 为负数
(check-catch 'out-of-range (substring-uncopied "01234" -1))
;; 错误：end 大于字符串长度
(check-catch 'out-of-range (substring-uncopied "01234" 1 6))
;; 错误：end 小于 start
(check-catch 'out-of-range (substring-uncopied "01234" 3 1))

(check-report)
