;; dollar
;; Scala 式链式调用语法糖：宏展开期去糖为原生函数嵌套调用。
;;
;; 语法
;; ----
;; ($ x)
;; ($ x :op args ... rest ...)
;;
;; 参数
;; ----
;; x : list? / vector? / string? / option?
;; 管道入口集合或 option。
;;
;; args ... : 与算子相关的参数
;; 各算子所需的谓词、函数、下标或元素，详见各算子测试文件。
;;
;; 返回值
;; ------
;; any
;; 链尾算子的结果，天然是裸值，无 :collect 拆箱步骤。
;;
;; 错误处理
;; --------
;; 1. 欠参拦截（Arity Mismatch）：不报未知算子，精准提示参数不足与正确用法；
;; 2. 标量延续拦截（Pipeline Type Error）：在 number/boolean 上继续接
;;    集合算子时给出明确定位与指引；
;; 3. 真正未知算子（Unknown Operator）：列出支持清单。
;;
;; 说明
;; ----
;; 纯 syntax-rules 宏，零装箱、零闭包对象、零方法分发；
;; 谓词位含 `_` 时自动提升为单参 lambda；
;; 未命中白名单的算子触发人性化报错。

(import (liii check) (liii lang) (liii option) (liii error) (scheme base))

(check-set-mode! 'report-failed)

;; === ($ x) 返回裸值，不装箱 ===
(check ($ '(1 2 3)) => '(1 2 3))
(check ($ 42) => 42)

;; === 链式组合 ===
(check ($ '(1 2 3 4 5) :filter odd? :map (lambda (x) (* x x))) => '(1 9 25))

;; === 1. 欠参拦截：单参算子未给参数 ===
(check-catch 'value-error ($ '(1 2 3) :map))
(check-catch 'value-error ($ '(1 2 3) :filter))
(check-catch 'value-error ($ '(1 2 3) :for-each))
(check-catch 'value-error ($ '(1 2 3) :take))
(check-catch 'value-error ($ '(1 2 3) :drop))
(check-catch 'value-error ($ '(1 2 3) :take-right))
(check-catch 'value-error ($ '(1 2 3) :drop-right))
(check-catch 'value-error ($ '(1 2 3) :take-while))
(check-catch 'value-error ($ '(1 2 3) :drop-while))
(check-catch 'value-error ($ '(1 2 3) :find))
(check-catch 'value-error ($ '(1 2 3) :partition))
(check-catch 'value-error ($ '(1 2 3) :contains))
(check-catch 'value-error ($ '(1 2 3) :index-of))
(check-catch 'value-error ($ '(1 2 3) :flat-map))
(check-catch 'value-error ($ '(1 2 3) :sort-with))
(check-catch 'value-error ($ '(1 2 3) :split-at))
(check-catch 'value-error ($ '(1 2 3) :starts-with?))
(check-catch 'value-error ($ '(1 2 3) :ends-with?))

;; === 2. 欠参拦截：二参算子缺少参数 ===
(check-catch 'value-error ($ '(1 2 3) :slice))
(check-catch 'value-error ($ '(1 2 3) :slice 1))
(check-catch 'value-error ($ '(1 2 3) :fold-left))
(check-catch 'value-error ($ '(1 2 3) :fold-left 0))
(check-catch 'value-error ($ '(1 2 3) :fold))
(check-catch 'value-error ($ '(1 2 3) :fold 0))

;; === 3. 非法链式类型延续：在数值上继续接集合算子 ===
;; :sum, :count, :length 返回数值标量，不可再接 :map / :filter 等
(check-catch 'type-error ($ '(1 2 3) :sum :map (* _ 2)))
(check-catch 'type-error ($ '(1 2 3) :count odd? :filter (even? _)))
(check-catch 'type-error ($ '(1 2 3) :length :take 2))

;; === 4. 非法链式类型延续：在布尔值上继续接集合算子 ===
;; 谓词返回 #t/#f，不可再接集合算子
(check-catch 'type-error ($ '(1 2 3) :contains 2 :filter odd?))
(check-catch 'type-error ($ '(1 2 3) :exists (> _ 0) :map (* _ 2)))
(check-catch 'type-error ($ '(1 2 3) :empty? :take 1))

;; === 5. 真正未知算子：列出支持列表 ===
(check-catch 'value-error ($ '(1 2 3) :definitely-not-an-operator))
(check-catch 'value-error ($ '(1 2 3) :flter odd?))

;; === 6. 语法格式错误：非关键字操作符（如多余参数或漏冒号） ===
;; :sum 是无参算子，传入 (+ _ 1) 会被判定为非关键字操作符语法错误
(check-catch 'value-error ($ '(1 2 3) :find odd? :sum (+ _ 1)))
(check-catch 'value-error ($ '(1 2 3) :sum (+ _ 1)))

;; === 7. Option 缺乏集合算子：option 没有 :sum ===
(check-catch 'type-error ($ (option 1) :sum))
(check-catch 'type-error ($ '(1 2 3) :find odd? :sum))
(check-catch 'type-error ($ (option 1) :sort-with <))
(check-catch 'type-error ($ (option 1) :take 1))

(check-report)
