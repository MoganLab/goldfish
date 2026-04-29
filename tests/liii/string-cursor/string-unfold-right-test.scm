(import (liii check))
(import (liii string-cursor))

(check-set-mode! 'report-failed)

;; string-unfold-right
;; 使用种子序列构造字符串（从右到左）。
;;
;; 语法
;; ----
;; (string-unfold-right p f g seed [base make-final])
;;
;; 参数
;; ----
;; p : procedure
;; 停止谓词，接收种子，返回真时停止
;;
;; f : procedure
;; 映射函数，将种子映射为字符
;;
;; g : procedure
;; 后继函数，生成下一个种子
;;
;; seed : any
;; 初始种子
;;
;; base : string (可选)
;; 后缀字符串，默认为空字符串
;;
;; make-final : procedure (可选)
;; 最终处理函数，接收最后一个种子并返回前缀字符串
;;
;; 返回值
;; ------
;; string?
;; 构造的新字符串
;;
;; 说明
;; ----
;; 1. 与 string-unfold 类似，但字符按相反顺序构建
;; 2. 与 (liii string) 的区别：无直接对应函数，(liii string-cursor) 独有
;; 3. 性能：O(n)，n 为生成的字符数

(check (string-unfold-right (lambda (n) (> n 2))
         (lambda (n) (integer->char (+ n 65)))
         (lambda (n) (+ n 1))
         0
       ) ;string-unfold-right
  =>
  "CBA"
) ;check

(check (string-unfold-right (lambda (n) (> n 2))
         (lambda (n) (integer->char (+ n 65)))
         (lambda (n) (+ n 1))
         0
         "x"
       ) ;string-unfold-right
  =>
  "CBAx"
) ;check

(check (string-unfold-right (lambda (n) (>= n 0))
         (lambda (n) #\a)
         (lambda (n) (+ n 1))
         0
       ) ;string-unfold-right
  =>
  ""
) ;check

(check-report)
