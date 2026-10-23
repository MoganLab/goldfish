(import (liii check))
(import (liii goldfmt format atom) (liii goldfmt record) (liii goldfmt scan))

(check-set-mode! 'report-failed)

;; reader-prefix-env?
;; 判断 node 是否为 reader 前缀表达式节点（tag 为 quasiquote、
;; unquote 或 unquote-splicing），stem-mode 下恒为 #f。
;;
;; 语法
;; ----
;; (reader-prefix-env? node)
;;
;; 参数
;; ----
;; node : any?
;;
;; 返回值
;; ------
;; boolean?

;; 测试：quasiquote 为 #t
(check (reader-prefix-env? (make-env :tag-name "quasiquote")) => #t)

;; 测试：unquote 为 #t
(check (reader-prefix-env? (make-env :tag-name "unquote")) => #t)

;; 测试：unquote-splicing 为 #t
(check (reader-prefix-env? (make-env :tag-name "unquote-splicing")) => #t)

;; 测试：其他 tag 为 #f
(check (reader-prefix-env? (make-env :tag-name "quote")) => #f)

;; 测试：stem-mode 下恒为 #f
(check (call-with-stem-mode
         (lambda () (reader-prefix-env? (make-env :tag-name "quasiquote"))))
  => #f
) ;check

(check-report)
