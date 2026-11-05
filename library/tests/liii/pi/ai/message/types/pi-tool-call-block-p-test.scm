;; pi-tool-call-block?
;; 判断对象是否为工具调用内容块 record 实例。
;;
;; 语法
;; ----
;; (pi-tool-call-block? obj)
;;
;; 参数
;; ----
;; obj : any?
;; 待判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若属于工具调用内容块 record 实例则返回 #t，否则返回 #f。

(import (liii check) (liii pi ai message types))

(check-set-mode! 'report-failed)

(let ((obj (%make-pi-tool-call-block "tc-001" "read" '(("path" . "foo.txt")) "sig-001")
      ) ;obj
     ) ;
  (check-true (pi-tool-call-block? obj))
) ;let
(check-false (pi-tool-call-block? '()))
(check-false (pi-tool-call-block? #f))
(check-false (pi-tool-call-block? "not-a-record"))

(check-report)
