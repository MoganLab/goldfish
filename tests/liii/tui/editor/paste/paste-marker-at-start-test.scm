;; paste-marker-at-start
;; 检测字符串在指定偏移位置是否起始于合法的粘贴标记。
;;
;; 语法
;; ----
;; (paste-marker-at-start reg line col)
;;
;; 参数
;; ----
;; reg : paste-registry?
;; 粘贴注册表实例。
;;
;; line : string?
;; 待检验的行文本。
;;
;; col : integer?
;; 字符起始偏移量。
;;
;; 返回值
;; ----
;; pair? 或 #f
;; 若起始于有效标记则返回 (marker . end-col)，否则返回 #f。

(import (scheme base) (liii check) (liii tui editor paste))

(check-set-mode! 'report-failed)

(let* ((reg (make-paste-registry))
       (m (paste-registry-add! reg (make-string 1001 #\a)))
       (line (string-append "pre" m "post"))
      ) ;
  (check (car (paste-marker-at-start reg line 3)) => m)
  (check-false (paste-marker-at-start reg line 0))
) ;let*

(check-report)
