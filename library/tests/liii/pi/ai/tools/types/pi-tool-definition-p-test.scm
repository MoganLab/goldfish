;; pi-tool-definition?
;; 判断对象是否为工具定义 record 实例。
;;
;; 语法
;; ----
;; (pi-tool-definition? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于工具定义 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai tools types))

(check-set-mode! 'report-failed)

(define exec-read-a (lambda (args) "ok"))
(let ((obj (%make-pi-tool-definition "read"
             "读取文件内容"
             '(("type" . "object"))
             exec-read-a
             "read file.txt"
             '("只读文件")
           ) ;%make-pi-tool-definition
      ) ;obj
     ) ;
  (check-true (pi-tool-definition? obj))
) ;let
(check-false (pi-tool-definition? '()))
(check-false (pi-tool-definition? #f))
(check-false (pi-tool-definition? "not-a-record"))

(check-report)
