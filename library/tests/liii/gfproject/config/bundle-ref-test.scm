;; bundle-ref
;; 从配置包 alist 中按键取值。
;;
;; 语法
;; ----
;; (bundle-ref bundle key)
;;
;; 参数
;; ----
;; bundle : list
;; 形如 (("key" . value) ...) 的配置包。
;; key : string
;; 要查找的键名。
;;
;; 返回值
;; ----
;; any 或 #f
;; 键对应的值；键不存在时返回 #f。

(import (liii check) (liii gfproject config))

(check-set-mode! 'report-failed)

(let ((bundle
        (list (cons "lib_config" '(("a" . 1))) (cons "merged_tools" '(("x" . 2))))
      ) ;bundle
     ) ;
  (check (bundle-ref bundle "lib_config") => '(("a" . 1)))
  (check (bundle-ref bundle "merged_tools") => '(("x" . 2)))
  ;; 键不存在
  (check-false (bundle-ref bundle "missing"))
) ;let

;; 空配置包
(check-false (bundle-ref '() "any"))

(check-report)
