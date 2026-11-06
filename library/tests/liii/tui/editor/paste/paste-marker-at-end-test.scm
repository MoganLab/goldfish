;; paste-marker-at-end
;; 检测字符串在指定偏移位置是否恰好结束于合法的粘贴标记。
;;
;; 语法
;; ----
;; (paste-marker-at-end reg line col)
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
;; 字符结束偏移量。
;;
;; 返回值
;; ----
;; pair? 或 #f
;; 若结束于有效标记则返回 (marker . start-col)，否则返回 #f。

(import (scheme base) (liii check) (liii tui editor paste))

(check-set-mode! 'report-failed)

(let* ((reg (make-paste-registry))
       (m (paste-registry-add! reg (make-string 1001 #\a)))
       (line (string-append "pre" m "post"))
       (end-col (+ 3 (string-length m)))
      ) ;
  (check (car (paste-marker-at-end reg line end-col)) => m)
  (check-false (paste-marker-at-end reg line 10))
) ;let*

(check-report)
