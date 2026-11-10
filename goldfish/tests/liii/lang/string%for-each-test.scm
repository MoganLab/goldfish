;; string%for-each
;; 遍历字符串执行副作用（对标 Scala String.foreach）。
;;
;; 语法
;; ----
;; ($ s :for-each f)
;;
;; 参数
;; ----
;; f : procedure?
;; 单参过程，按字符顺序施加副作用。
;;
;; s : string?
;; 源字符串。
;;
;; 返回值
;; ------
;; string?
;; 返回原字符串 s 本身（保持可链），副作用经由闭包观察。
;;
;; 描述
;; ----
;; UTF-8 语义，基于 (liii string-cursor) 的 string-for-each-cursor。

(import (liii check) (liii lang) (scheme base))

(check-set-mode! 'report-failed)

(let ((acc 0))
  (check
    ($ "测试🎉" :for-each (lambda (c) (set! acc (+ acc 1))))
    =>
    "测试🎉"
  ) ;check
  (check acc => 3)
) ;let
(check ($ "" :for-each (lambda (c) c)) => "")

(check-report)
