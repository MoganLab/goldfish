;; pretty
;; 按 Scheme 排版习惯美化输出 S 表达式的组合子。
;;
;; 语法
;; ----
;; (pretty obj)
;;
;; 参数
;; ----
;; obj : any
;; 待排版的对象。
;;
;; 返回值
;; ----
;; format?
;; 输出美化后代码文本并补一个换行的格式化组合子。
;;
;; 说明
;; ----
;; 排版遵循 define、lambda、let 等关键字的缩进惯例，宽度
;; 不足时才换行对齐；共享与循环结构会标注 `#n=` / `#n#`。
;; `(liii format pretty)` 以 Goldfish 命名重导出本函数。

(import (liii check) (srfi srfi-166 base) (srfi srfi-166 pretty))

(check-set-mode! 'report-failed)

(check (show #f (pretty '(foo bar))) => "(foo bar)\n")
(check (show #f (pretty 42)) => "42\n")
(check (show #f (pretty '"a b")) => "\"a b\"\n")
(check
  (show #f
    (pretty
      '(define (fold kons knil ls)
         (define (loop ls acc)
           (if (null? ls) acc (loop (cdr ls) (kons (car ls) acc))))
         (loop ls knil))
    ) ;pretty
  ) ;show
  =>
  "(define (fold kons knil ls)\n  (define (loop ls acc)\n    (if (null? ls) acc (loop (cdr ls) (kons (car ls) acc))))\n  (loop ls knil))\n"
) ;check

(check-report)
