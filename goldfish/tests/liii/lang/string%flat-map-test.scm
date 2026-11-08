;; string%flat-map
;; 字符串映射后展平（对标 Scala String.flatMap）。
;;
;; 语法
;; ----
;; ($ s :flat-map f)
;;
;; 参数
;; ----
;; f : procedure?
;; 单参字符函数，返回字符/字符串/字符集合。
;;
;; 返回值
;; ------
;; string?
;; 各结果按序拼接。UTF-8 语义。
;;
;;   => ($ "ab" :flat-map (lambda (c) (list c c)))  ; "aabb"

(import (liii check) (liii lang))

(check-set-mode! 'report-failed)

(check ($ "ab" :flat-map (lambda (c) (list c c))) => "aabb")
(check ($ "" :flat-map (lambda (c) (list c))) => "")
(check ($ "测试" :flat-map (lambda (c) (list c))) => "测试")

(check-report)
