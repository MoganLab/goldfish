;; higher_order.scm: 高阶函数管道 (map / filter) 回调执行出错的调用栈回溯演示

(import (scheme base) (liii list) (liii string))

;; 步骤 1：解析单行 CSV 记录

(define (parse-line line)
  (let ((parts (string-split line ",")))
    (cons "parsed" parts)
  ) ;let
) ;define

;; 步骤 2：提取年龄字段

(define (extract-age record)
  (let ((fields (cdr record)))
    ;; 如果记录缺少年龄字段，(cadr fields) 会因为 fields 只有一个元素而导致 (car '()) 报错
    (cons "age" (car (cdr fields)))
  ) ;let
) ;define

;; 步骤 3：数据清洗流水线，使用 map 对记录进行批量转换

(define (pipeline lines)
  (let* ((records (map parse-line lines)) (ages (map extract-age records)))
    (cons "result" ages)
  ) ;let*
) ;define

;; 批处理数据：第 2 条数据 "bob" 损坏（缺少逗号与年龄字段），触发流水线崩溃
(pipeline '("alice,30" "bob" "charlie,25"))
