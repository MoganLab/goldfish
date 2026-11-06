;; make-env
;; 创建一个环境记录对象，用于表示格式化过程中的环境上下文。
;;
;; 语法
;; ----
;; (make-env [arg ...])
;;
;; 参数
;; ----
;; tag-name : string? 可选，默认值为 ""
;; 标记名称，空字符串 "" 表示无标签。
;;
;; depth : integer? 可选，默认值为 0
;; 标记的深度（嵌套层级）。
;;
;; indent : integer? 可选，默认值为 -1
;; 标记的缩进量（空格数）。-1 表示缩进尚未确认。
;;
;; children : vector? 可选，默认值为 (vector)
;; 子环境列表，元素为 env 记录。
;;
;; left-line : integer? 可选，默认值为 0
;; 左侧标记所在的行号，0 表示行号未确定。
;;
;; right-line : integer? 可选，默认值为 0
;; 右侧标记所在的行号，0 表示行号未确定。
;;
;; value : any? 可选，默认值为 #f
;; 环境的值。
;;
;; 返回值
;; ----
;; env?
;; 新创建的环境记录对象。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; 测试默认参数
(check (env-tag-name (make-env)) => "")
(check (env-depth (make-env)) => 0)
(check (env-indent (make-env)) => -1)
(check (env-children (make-env)) => (vector))
(check (env-left-line (make-env)) => 0)
(check (env-right-line (make-env)) => 0)
(check (env-value (make-env)) => #f)

;; 测试指定部分具名参数
(check (env-tag-name (make-env :tag-name "lambda")) => "lambda")
(check (env-depth (make-env :depth 5)) => 5)
(check (env-indent (make-env :indent 4)) => 4)
(check (env-left-line (make-env :left-line 10)) => 10)
(check (env-right-line (make-env :right-line 20)) => 20)
(check (env-value (make-env :value '(define x 1))) => '(define x 1))

;; 测试 tag-name 接受 symbol 并转换为字符串
(check (env-tag-name (make-env :tag-name 'define)) => "define")

;; 测试 children 接受列表并转换为向量
(check (env-children (make-env :children '(a b))) => (vector 'a 'b))

;; 测试非法参数抛出 value-error
(check-catch 'value-error (make-env :depth -1))
(check-catch 'value-error (make-env :indent -2))
(check-catch 'value-error (make-env :left-line -1))
(check-catch 'value-error (make-env :right-line -1))
(check-catch 'value-error (make-env :tag-name 42))
(check-catch 'value-error (make-env :children "not-a-vector"))

(check-report)
