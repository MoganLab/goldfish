(import (liii check))
(import (liii goldfmt-scan))
(import (liii goldfmt-record))

(check-set-mode! 'report-failed)

;; scan
;; 将 Scheme datum 扫描成 formatter 使用的 node 树。
;;
;; 语法
;; ----
;; (scan datum)
;;
;; 参数
;; ----
;; datum : any?
;; 已经由 `read` 读出的 Scheme datum。
;;
;; 返回值
;; ------
;; env? 或 atom?
;; 如果 datum 是原子值，返回 atom。
;; 如果 datum 是 list，返回 env。
;;
;; 说明
;; ----
;; 1. symbol、number、string、boolean、char、空列表和 vector 会被扫描成 atom
;; 2. 非空 list 会被扫描成 env
;; 3. list 的第一个元素是 symbol 时，它会成为 env 的 tag-name
;; 4. list 的第一个元素不是 symbol 时，env 的 tag-name 为空字符串
;; 5. scan 只负责结构转换，不负责格式化和位置信息
;;
;; 示例
;; ----
;; (scan '(define x 1)) ; => env，tag-name 为 "define"
;; (scan 'x)            ; => atom，value 为 'x

;; 测试 atom 输入 - 符号
(let ((result (scan 'x)))
  (check (atom? result) => #t)
  (check (atom-value result) => 'x))

;; 测试 atom 输入 - 整数
(let ((result (scan 42)))
  (check (atom? result) => #t)
  (check (atom-value result) => 42))

;; 测试 atom 输入 - 浮点数
(let ((result (scan 3.14)))
  (check (atom? result) => #t)
  (check (atom-value result) => 3.14))

;; 测试 atom 输入 - 字符串
(let ((result (scan "hello")))
  (check (atom? result) => #t)
  (check (atom-value result) => "hello"))

;; 测试 atom 输入 - 布尔值
(let ((result (scan #t)))
  (check (atom? result) => #t)
  (check (atom-value result) => #t))

(let ((result (scan #f)))
  (check (atom? result) => #t)
  (check (atom-value result) => #f))

;; 测试 atom 输入 - 字符
(let ((result (scan #\a)))
  (check (atom? result) => #t)
  (check (atom-value result) => #\a))

(let ((result (scan #\newline)))
  (check (atom? result) => #t)
  (check (atom-value result) => #\newline))

;; 测试 atom 输入 - 空列表（空列表没有右侧标记，是 atom）
(let ((result (scan '())))
  (check (atom? result) => #t)
  (check (atom-value result) => '()))

;; 测试简单 list 输入
(let ((result (scan '(define x 1))))
  (check (env? result) => #t)
  (check (env-tag-name result) => "define")
  (check (env-depth result) => 0)
  (check (vector-length (env-children result)) => 2))

;; 测试嵌套结构
(let ((result (scan '(if (a b) c d))))
  (check (env? result) => #t)
  (check (env-tag-name result) => "if")
  (check (env-depth result) => 0)
  (check (vector-length (env-children result)) => 3)
  ;; 检查第一个子元素是嵌套的 env
  (let ((first-child (vector-ref (env-children result) 0)))
    (check (env? first-child) => #t)
    (check (env-tag-name first-child) => "a")
    (check (env-depth first-child) => 1)))

;; 测试深度嵌套
(let ((result (scan '(a (b (c d))))))
  (check (env? result) => #t)
  (check (env-tag-name result) => "a")
  (check (env-depth result) => 0)
  ;; 检查第二层嵌套
  (let ((first-child (vector-ref (env-children result) 0)))
    (check (env? first-child) => #t)
    (check (env-tag-name first-child) => "b")
    (check (env-depth first-child) => 1)
    ;; 检查第三层嵌套
    (let ((grandchild (vector-ref (env-children first-child) 0)))
      (check (env? grandchild) => #t)
      (check (env-tag-name grandchild) => "c")
      (check (env-depth grandchild) => 2))))

;; 测试 quote 列表 '(1 2 3) -> (quote (1 2 3))
(let ((result (scan '(1 2 3))))
  (check (env? result) => #t)
  (check (env-tag-name result) => "")   ; 第一个元素 1 不是 symbol，所有元素都是 children
  (check (env-depth result) => 0)
  (check (vector-length (env-children result)) => 3)  ; 1, 2, 3 都是 children
  ;; 检查子元素
  (let ((first-child (vector-ref (env-children result) 0)))
    (check (atom? first-child) => #t)
    (check (atom-value first-child) => 1)  ; 第一个 child 是 1
    (check (atom-depth first-child) => 1)))

;; 测试显式 quote 形式 (quote (1 2 3))
;; quote 形式整个作为一个 env，没有 children
(let ((result (scan '(quote (1 2 3)))))
  (check (env? result) => #t)
  (check (env-tag-name result) => "quote")   ; quote 符号转换为字符串
  (check (env-depth result) => 0)
  (check (vector-length (env-children result)) => 0)  ; quote 形式没有 children
  (check (env-value result) => '(quote (1 2 3))))

;; 测试 vector 输入 #(1 2 3)
(let ((result (scan '#(1 2 3))))
  (check (atom? result) => #t)
  (check (atom-value result) => #(1 2 3))
  (check (atom-depth result) => 0))

;; 测试空 vector #()
(let ((result (scan '#())))
  (check (atom? result) => #t)
  (check (atom-value result) => #())
  (check (atom-depth result) => 0))

;; 测试 quote 简写 'x - 注意：'x 求值后是 symbol x，不是 list
(let ((result (scan 'x)))
  (check (atom? result) => #t)
  (check (atom-value result) => 'x)
  (check (atom-depth result) => 0))

;; 非法的多参数 quote 不应被识别成 quote 特殊形式
(let ((result (scan '(quote x y))))
  (check (env? result) => #t)
  (check (env-tag-name result) => "quote")
  (check (env-depth result) => 0)
  (check (vector-length (env-children result)) => 2)
  (check (env-value result) => '(quote x y)))

;; 测试语法关键字 #_quote 作为 tag-name
;; quote 形式整个作为一个 env，没有 children
(let ((result (scan '(#_quote x))))
  (check (env? result) => #t)
  (check (env-tag-name result) => "#_quote")
  (check (env-depth result) => 0)
  (check (vector-length (env-children result)) => 0)  ; quote 形式没有 children
  (check (env-value result) => '(#_quote x)))

;; 测试 let 表达式，绑定列表 ((x 1)) 的 tag-name 为空字符串
(let ((result (scan '(let ((x 1)) (+ x 1)))))
  (check (env? result) => #t)
  (check (env-tag-name result) => "let")
  (check (env-depth result) => 0)
  (check (vector-length (env-children result)) => 2)
  ;; 检查第一个子元素是绑定列表 ((x 1))
  ;; ((x 1)) 的第一个元素 (x 1) 是列表不是 symbol，所以 (x 1) 作为 child
  (let ((bindings (vector-ref (env-children result) 0)))
    (check (env? bindings) => #t)
    (check (env-tag-name bindings) => "")    ; 第一个元素 (x 1) 是列表，不是 symbol
    (check (env-depth bindings) => 1)
    (check (vector-length (env-children bindings)) => 1)  ; (x 1) 是唯一的 child
    ;; 检查绑定 (x 1)
    (let ((binding (vector-ref (env-children bindings) 0)))
      (check (env? binding) => #t)
      (check (env-tag-name binding) => "x")   ; x 是 symbol
      (check (env-depth binding) => 2)
      (check (vector-length (env-children binding)) => 1)  ; 1 是 child
      (let ((value (vector-ref (env-children binding) 0)))
        (check (atom? value) => #t)
        (check (atom-value value) => 1)))))

(check-report)
