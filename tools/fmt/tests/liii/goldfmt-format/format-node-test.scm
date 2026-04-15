(import (liii check)
        (liii goldfmt-format)
        (liii goldfmt-record)
        (liii goldfmt-scan)
        (liii raw-string))

(check-set-mode! 'report-failed)

(define (format-node-text node column)
  (call-with-values
    (lambda () (format-node node column))
    (lambda (text new-node) text)))

;; format-node
;; 从指定列开始格式化一个 node。
;;
;; 语法
;; ----
;; (format-node node column)
;;
;; 参数
;; ----
;; node : env? 或 atom?
;; 由 `scan` 生成的格式化节点。
;;
;; column : integer?
;; 当前 node 在当前行开始输出的列号，从 0 开始计数。
;;
;; 返回值
;; ------
;; values
;; 返回两个值：
;; 1. text : string? 格式化后的文本
;; 2. positioned-node : env? 或 atom? 带有 indent、left-line、right-line 的新 node
;;
;; 说明
;; ----
;; 1. `format-node` 是 formatter 的核心入口
;; 2. 原始 node 不会被修改
;; 3. 位置信息写入返回的新 node 树
;; 4. 当 node 可以 inline 时输出单行，否则调用跨行 env 渲染流程
;;
;; 示例
;; ----
;; (call-with-values
;;   (lambda () (format-node (scan '(define x 1)) 0))
;;   (lambda (text positioned-node) text))

(check (format-node-text (scan '(define x 1)) 0)
       => "(define x 1)")

(call-with-values
  (lambda () (format-node (scan 'answer) 4))
  (lambda (text node)
    (check text => "answer")
    (check (atom-indent node) => 4)
    (check (atom-left-line node) => 1)
    (check (atom-right-line node) => 1)))

(call-with-values
  (lambda () (format-node (scan '(*comment* "hello")) 0))
  (lambda (text node)
    (check text => ";; hello")
    (check (env-indent node) => 0)
    (check (env-left-line node) => 1)
    (check (env-right-line node) => 1)
    (let ((content (vector-ref (env-children node) 0)))
      (check (atom-indent content) => 3)
      (check (atom-left-line content) => 1)
      (check (atom-right-line content) => 1))))

(call-with-values
  (lambda () (format-node (scan '(*comment* "")) 2))
  (lambda (text node)
    (check text => ";;")
    (check (env-indent node) => 2)
    (let ((content (vector-ref (env-children node) 0)))
      (check (atom-indent content) => 4))))

(check (format-node-text (scan '(begin (display "x") (newline))) 2)
       => (&- "
            (begin
                (display \"x\")
                (newline)
              ) ;begin
            "))

(check (format-node-text (scan '(if (very-long-predicate-name x)
                                    (compute-true-branch x)
                                    (compute-false-branch x)))
                         4)
       => (&- "
            (if (very-long-predicate-name x)
                  (compute-true-branch x)
                  (compute-false-branch x)
                ) ;if
            "))

(check-report)
