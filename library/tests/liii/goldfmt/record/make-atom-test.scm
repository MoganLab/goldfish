;; make-atom
;; 创建一个 atom 记录对象，用于表示字符串、数字、symbol 等无括号结构。
;;
;; 语法
;; ----
;; (make-atom [arg ...])
;;
;; 参数
;; ----
;; depth : integer? 可选，默认值为 0
;; 节点的深度。
;;
;; indent : integer? 可选，默认值为 -1
;; 节点的缩进量，-1 表示缩进尚未确认。
;;
;; left-line : integer? 可选，默认值为 0
;; atom 起始文本所在行，0 表示行号未确定。
;;
;; right-line : integer? 可选，默认值为 0
;; atom 结束文本所在行，0 表示行号未确定。
;;
;; value : any? 可选，默认值为 #f
;; atom 保存的原始 datum 值。
;;
;; 返回值
;; ----
;; atom?
;; 新创建的 atom 记录对象。

(import (liii check) (liii goldfmt record))

(check-set-mode! 'report-failed)

;; 测试默认参数
(check (atom-depth (make-atom)) => 0)
(check (atom-indent (make-atom)) => -1)
(check (atom-left-line (make-atom)) => 0)
(check (atom-right-line (make-atom)) => 0)
(check (atom-value (make-atom)) => #f)

;; 测试指定全部参数
(let ((a (make-atom :depth 1 :indent 2 :left-line 3 :right-line 4 :value "x")))
  (check (atom-depth a) => 1)
  (check (atom-indent a) => 2)
  (check (atom-left-line a) => 3)
  (check (atom-right-line a) => 4)
  (check (atom-value a) => "x")
) ;let

;; 测试非法参数抛出 value-error
(check-catch 'value-error (make-atom :depth -1))
(check-catch 'value-error (make-atom :indent -2))
(check-catch 'value-error (make-atom :left-line -1))
(check-catch 'value-error (make-atom :right-line -1))

(check-report)
