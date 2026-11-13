;; make-bigint
;; 创建规范化的 bigint 记录。
;;
;; 语法
;; ----
;; (make-bigint signum mag)
;;
;; 参数
;; ----
;; signum : integer?
;;   符号：-1 表示负数，0 表示零，1 表示正数。
;; mag : list?
;;   magnitude 数组，每个元素是 0 到 BASE-1 的整数，大端序。
;;
;; 返回值
;; ----
;; bigint?
;;   新创建的 bigint 记录。

(import (liii check)
        (liii bigint record)
) ;import

(check-set-mode! 'report-failed)

;; 创建零
(let ((z (make-bigint 0 '())))
  (check-true (bigint? z))
  (check (bigint-signum z) => 0)
  (check (bigint-mag z) => '())
) ;let

;; 创建正数
(let ((p (make-bigint 1 '(100 200))))
  (check-true (bigint? p))
  (check (bigint-signum p) => 1)
  (check (bigint-mag p) => '(100 200))
) ;let

;; 创建负数
(let ((n (make-bigint -1 '(300))))
  (check-true (bigint? n))
  (check (bigint-signum n) => -1)
  (check (bigint-mag n) => '(300))
) ;let

;; 自动规范化（去除前导零）
(let ((n (make-bigint 1 '(0 0 123))))
  (check (bigint-mag n) => '(123))
) ;let

;; 全零转为零
(let ((n (make-bigint 1 '(0 0 0))))
  (check (bigint-signum n) => 0)
  (check (bigint-mag n) => '())
) ;let

;; 错误：非法符号
(check-catch 'value-error (make-bigint 2 '(123)))
(check-catch 'value-error (make-bigint -2 '(123)))

;; 错误：signum=0 但 mag 非零
(check-catch 'value-error (make-bigint 0 '(123)))

;; 错误：mag 元素超出范围
(check-catch 'value-error (make-bigint 1 '(1000000000000)))

(check-report)
