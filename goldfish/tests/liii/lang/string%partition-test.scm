;; string%partition
;; 按谓词拆分字符串为两串（对标 Scala String.partition）。
;;
;; 语法
;; ----
;; ($ s :partition p)
;;
;; 参数
;; ----
;; p : procedure?
;; 字符谓词；可用 `_` 占位符简写。
;;
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; list?
;; 二元组 ("满足字符..." "不满足字符...")，UTF-8 语义。

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "aXbX" :partition (lambda (c) (char=? c #\X))) => '("XX" "ab"))
(check ($ "测试法" :partition (lambda (c) (char=? c #\法)))
  =>
  '("法" "测试")
) ;check
(check ($ "" :partition (lambda (c) #t)) => '("" ""))

(check-report)
