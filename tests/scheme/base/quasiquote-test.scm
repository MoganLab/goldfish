(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; quasiquote
;; 创建带有可求值部分的列表或向量模板。
;;
;; 语法
;; ----
;; `template
;; (quasiquote template)
;;
;; 参数
;; ----
;; template : 任意类型
;; 模板表达式。
;;
;; 返回值
;; ------
;; 任意类型
;; 构建的数据结构。
;;
;; 说明
;; ----
;; 1. 模板中大部分内容原样保留
;; 2. 使用 unquote (,) 求值部分表达式
;; 3. 使用 unquote-splicing (,@) 将列表展开插入
;; 4. 反引号 ` 是 quasiquote 的简写
(let ((x 2) (y 3))
  (check `(list ,x ,y) => '(list 2 3))
) ;let
(let ((lst '(b c)))
  (check `(a ,@lst d) => '(a b c d))
) ;let
(check `(1 2 3) => '(1 2 3))
(check `hello => 'hello)

(check-report)
