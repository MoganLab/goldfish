(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-cursor-start
;; 返回字符串的起始游标。
;;
;; 语法
;; ----
;; (string-cursor-start s)
;;
;; 参数
;; ----
;; s : string
;; 目标字符串
;;
;; 返回值
;; ------
;; string-cursor?
;; 指向字符串第一个字符之前位置的游标
;;
;; 说明
;; ----
;; 1. 创建游标时会预扫描字符串生成偏移表
;; 2. 空字符串返回指向位置0的游标
;; 3. 性能：O(n)，n为字符串字节长度（预扫描一次）

;; 测试空字符串
(let ((c (string-cursor-start "")))
  (check (string-cursor? c) => #t)
  (check (string-cursor->index "" c) => 0))

;; 测试ASCII字符串
(let ((c (string-cursor-start "abc")))
  (check (string-cursor? c) => #t)
  (check (string-cursor->index "abc" c) => 0))

;; 测试中文字符串
(let ((c (string-cursor-start "中文")))
  (check (string-cursor? c) => #t)
  (check (string-cursor->index "中文" c) => 0))

;; 测试emoji字符串
(let ((c (string-cursor-start "🎉🎊")))
  (check (string-cursor? c) => #t)
  (check (string-cursor->index "🎉🎊" c) => 0))

(check-report)
