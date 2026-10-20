(import (liii check)
        (liii tui terminal))

(check-set-mode! 'report-failed)

;; terminal-detect-size
;; 探测终端窗口尺寸（列数与行数）。
;;
;; 语法
;; ----
;; (terminal-detect-size [getenv-fn])
;;
;; 参数
;; ----
;; getenv-fn : procedure, 可选
;; 环境变量获取过程，接收环境变量名字符串，返回字符串或 #f。
;; 缺省时使用系统 getenv 函数。
;;
;; 返回值
;; ----
;; list of integer
;; 格式为 (width height)，即 '(列数 行数)。默认回退为 '(80 24)。

;; 默认情况（或当 getenv 全部返回 #f 时），稳定返回 80x24
(let ((fake-env (lambda (k) #f)))
  (check (terminal-detect-size fake-env) => '(80 24)))

;; 正常解析合法的 COLUMNS 与 LINES
(let ((fake-env (lambda (k)
                  (cond ((string=? k "COLUMNS") "120")
                        ((string=? k "LINES") "40")
                        (else #f)))))
  (check (terminal-detect-size fake-env) => '(120 40)))

;; 仅 COLUMNS 有效，LINES 缺失时，LINES 回退为 24
(let ((fake-env (lambda (k)
                  (cond ((string=? k "COLUMNS") "100")
                        (else #f)))))
  (check (terminal-detect-size fake-env) => '(100 24)))

;; 仅 LINES 有效，COLUMNS 缺失时，COLUMNS 回退为 80
(let ((fake-env (lambda (k)
                  (cond ((string=? k "LINES") "50")
                        (else #f)))))
  (check (terminal-detect-size fake-env) => '(80 50)))

;; 非法字符（非正整数），安全回退默认值
(let ((fake-env (lambda (k)
                  (cond ((string=? k "COLUMNS") "invalid")
                        ((string=? k "LINES") "abc")
                        (else #f)))))
  (check (terminal-detect-size fake-env) => '(80 24)))

;; 非正整数（0 或负数），安全回退默认值
(let ((fake-env (lambda (k)
                  (cond ((string=? k "COLUMNS") "0")
                        ((string=? k "LINES") "-10")
                        (else #f)))))
  (check (terminal-detect-size fake-env) => '(80 24)))

;; 无参调用（系统环境探测）：返回包含两个正整数的列表
(let ((size (terminal-detect-size)))
  (check (pair? size) => #t)
  (check (length size) => 2)
  (check (> (car size) 0) => #t)
  (check (> (cadr size) 0) => #t))

(check-report)
