;; paste-registry-remove!
;; 从粘贴注册表中删除指定标记对应的项。
;;
;; 语法
;; ----
;; (paste-registry-remove! reg marker)
;;
;; 参数
;; ----
;; reg : paste-registry?
;; 粘贴注册表实例。
;;
;; marker : string?
;; 标记占位符。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui editor paste))

(check-set-mode! 'report-failed)

(let* ((reg (make-paste-registry))
       (text (make-string 1001 #\a))
       (m (paste-registry-add! reg text))
      ) ;
  (check (paste-expand reg m) => text)
  (paste-registry-remove! reg m)
  ;; 移除后不再展开
  (check (paste-expand reg m) => m)
) ;let*

(check-report)
