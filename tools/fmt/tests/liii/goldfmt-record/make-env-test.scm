(import (liii check))
(import (liii goldfmt-record))

(check-set-mode! 'report-failed)

;; make-env
;; 创建一个环境记录对象，用于表示格式化过程中的环境上下文。
;;
;; 语法
;; ----
;; (make-env [arg ...])
;;
;; 具名参数
;; ------
;; tag-name : string? 可选，默认值为 ""
;; 标记名称，空字符串 "" 表示无标签。
;;
;; depth : integer? 可选，默认值为 0
;; 标记的深度（嵌套层级）。
;;
;; indent : integer? 可选，默认值为 -1
;; 标记的缩进量（空格数）。`-1` 表示缩进尚未确认。
;;
;; children : vector? 可选，默认值为 (vector)
;; 子环境列表，vector of env。
;;
;; left-line : integer? 可选，默认值为 0
;; 左侧标记所在的行号。`0` 表示行号未确定。
;;
;; right-line : integer? 可选，默认值为 0
;; 右侧标记所在的行号。`0` 表示行号未确定。
;;
;; value : any? 可选，默认值为 #f
;; 环境的值。
;;
;; 返回值
;; ------
;; env?
;; 新创建的环境记录对象。
;;
;; 说明
;; ----
;; 1. 使用具名参数可以只指定部分参数，其余使用默认值
;; 2. 子环境的深度必须等于父环境深度 + 1
;; 3. 深度为 0 的父节点，其缩进必须是 0
;;
;; 示例
;; ----
;; (make-env)                          ; 使用所有默认值
;; (make-env :tag-name "if")           ; 指定标签
;; (make-env :depth 1 :indent 2)       ; 指定深度和缩进
;; (make-env :tag-name "begin"
;;           :depth 0
;;           :indent 0
;;           :children (vector child-env)
;;           :left-line 5
;;           :right-line 10)

;; 测试默认参数
(check (env-tag-name (make-env)) => "")
(check (env-depth (make-env)) => 0)
(check (env-indent (make-env)) => -1)
(check (env-children (make-env)) => (vector))
(check (env-left-line (make-env)) => 0)
(check (env-right-line (make-env)) => 0)
(check (env-value (make-env)) => #f)

;; 测试指定所有参数
(check (env-tag-name (make-env :tag-name "if" :depth 1 :indent 2 :children (vector) :left-line 3 :right-line 5)) => "if")
(check (env-depth (make-env :tag-name "if" :depth 1 :indent 2 :children (vector) :left-line 3 :right-line 5)) => 1)
(check (env-indent (make-env :tag-name "if" :depth 1 :indent 2 :children (vector) :left-line 3 :right-line 5)) => 2)
(check (env-left-line (make-env :tag-name "if" :depth 1 :indent 2 :children (vector) :left-line 3 :right-line 5)) => 3)
(check (env-right-line (make-env :tag-name "if" :depth 1 :indent 2 :children (vector) :left-line 3 :right-line 5)) => 5)

;; 测试指定部分参数
(check (env-tag-name (make-env :tag-name "lambda")) => "lambda")
(check (env-depth (make-env :depth 5)) => 5)
(check (env-indent (make-env :indent 4)) => 4)
(check (env-left-line (make-env :left-line 10)) => 10)
(check (env-right-line (make-env :right-line 20)) => 20)
(check (env-value (make-env :value '(define x 1))) => '(define x 1))

;; 测试空标签
(check (env-tag-name (make-env :tag-name "")) => "")

;; 测试带子环境
(let* ((child (make-env :tag-name "child" :depth 1 :indent 2))
       (parent (make-env :tag-name "parent" :depth 0 :indent 0 :children (vector child))))
  (check (vector-length (env-children parent)) => 1)
  (check (env-tag-name (vector-ref (env-children parent) 0)) => "child"))

;; make-atom
;; 创建一个 atom 记录对象，用于表示字符串、数字、symbol 等无括号结构。
;;
;; 语法
;; ----
;; (make-atom [arg ...])
;;
;; 具名参数
;; ------
;; depth : integer? 可选，默认值为 0
;; 节点的深度。
;;
;; indent : integer? 可选，默认值为 -1
;; 节点的缩进量。`-1` 表示缩进尚未确认。
;;
;; left-line : integer? 可选，默认值为 0
;; atom 起始文本所在行。`0` 表示行号未确定。
;;
;; right-line : integer? 可选，默认值为 0
;; atom 结束文本所在行。`0` 表示行号未确定。
;;
;; value : any? 可选，默认值为 #f
;; atom 保存的原始 datum 值。
;;
;; 返回值
;; ------
;; atom?
;; 新创建的 atom 记录对象。
;;
;; 说明
;; ----
;; atom 没有 children，也没有 tag-name。
;; formatter 会在返回的新 atom 中填入 indent、left-line 和 right-line。
;;
;; 示例
;; ----
;; (make-atom :depth 0 :value 'x)

(let ((a (make-atom :depth 0 :indent 0 :left-line 1 :right-line 1 :value "hello")))
  (check (atom-depth a) => 0)
  (check (atom-indent a) => 0)
  (check (atom-left-line a) => 1)
  (check (atom-right-line a) => 1)
  (check (atom-value a) => "hello")
  (check (atom? a) => #t))

(check-report)

;; 来自: gf doc liii/goldfmt-record "make-env"
