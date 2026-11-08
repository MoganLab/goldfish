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
;; 谓词位含 `_` 时自动提升为单参 lambda，多个 _ 绑定同一参数；
;; 支持扁平（_ 在第 1~5 实参位）、深层（含 _ 子表达式在第 1~3 位）、
;; 恒等（裸 _）三种形态；超出枚举位的形态在运行时报
;; unbound variable _，请改用显式 lambda；
;; 不含 _ 的谓词位（如 odd?、现成 lambda）原样传递；
;; 未命中白名单的算子触发人性化报错。

(import (liii check) (liii lang) (liii option) (liii error) (scheme base)
  (scheme char))

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

;; === 8. `_` 占位符自动提升：扁平（_ 在各实参位）===
(check ($ '(1 2 3) :map (+ _ 2)) => '(3 4 5))
(check ($ '(1 2 3) :map (- 10 _)) => '(9 8 7))
(check ($ '(1 2 3) :map (max 7 _ 1)) => '(7 7 7))
(check ($ '(1 2 3) :map (expt 2 _)) => '(2 4 8))
(check ($ '("a" "b") :map (string-append "x" _)) => '("xa" "xb"))

;; === 9. `_` 深层嵌套 ===
(check ($ '(1 2 3) :map (+ (* _ 2) 1)) => '(3 5 7))
(check ($ '(1 2 3) :map (* (+ _ 1) 2)) => '(4 6 8))
(check ($ '(1 2 3) :map (list (* _ 2) (+ _ 1))) => '((2 2) (4 3) (6 4)))

;; === 10. 多个 _ 绑定同一参数 ===
(check ($ '(1 2 3) :map (list 0 _ _ 9)) => '((0 1 1 9) (0 2 2 9) (0 3 3 9)))

;; === 11. 恒等：裸 _ ===
(check ($ '(1 2 3) :map _) => '(1 2 3))
(check ($ #(1 2 3) :map _) => #(1 2 3))

;; === 12. 无 _ 原样传递 ===
(check ($ '(1 2 3 4) :filter odd?) => '(1 3))
(check ($ '(1 -2 3) :map abs) => '(1 2 3))
(check ($ '(1 2 3) :map (lambda (x) (* x 10))) => '(10 20 30))

;; === 13. if 表达式 / 谓词形态 ===
(check ($ '(1 2 3) :map (if (odd? _) 'o 'e)) => '(o e o))
(check ($ '(1 2 3 4) :filter (<= _ 3)) => '(1 2 3))
(check ($ '(1 2 3 4) :count (odd? _)) => 2)

;; === 14. vector 与 _ 组合 ===
(check ($ #(1 2 3) :map (* _ 2)) => #(2 4 6))
(check ($ #(1 2 3 4) :filter (odd? _)) => #(1 3))
(check ($ #(1 2 3 4 5) :filter (<= _ 3) :map (* _ 10) :sum) => 60)

;; === 15. 用户环境闭包捕获 ===
(let ((base 100))
  (check ($ '(1 2 3) :map (+ base _)) => '(101 102 103))
) ;let

;; === 16. 卫生性：调用点 shadow 不影响展开 ===
(let ((map (lambda args 'hijacked)))
  (check ($ '(1 2 3) :map (lambda (x) (+ x 1))) => '(2 3 4))
  (check ($ '(1 2 3) :map (+ _ 1)) => '(2 3 4))
) ;let

;; === 17. 用户环境闭包 ===
(let ((n 10))
  (check ($ '(1 2 3) :map (lambda (x) (+ x n))) => '(11 12 13))
) ;let

;; === 18. 明确排除的算子（by design）→ 人性化报错 ===
;; :collect 链尾已是裸值无需拆箱；:mk-string 属于带参算子不能无参调用
(check-catch 'value-error ($ '(1 2 3) :collect))

;; === 19. 长链组合：四算子 ===
;; 过滤偶数 → 平方 → 排序 → 求和
(check ($ '(5 2 8 1 9 4) :filter even? :map (* _ _) :sort-with < :sum) => 84)
;; 取前缀 → 去尾 → 去重 → 转向量
(check ($ '(1 1 2 2 3 3 4) :take 6 :drop 1 :distinct :to-vector) => #(1 2 3))
;; 映射 → 满足前缀 → 计数
(check ($ '(1 2 3 4 5 6) :map (+ _ 1) :take-while (<= _ 4) :count even?) => 2)
;; 字符串链：过滤 → 反转 → 取前缀 → 长度
(check ($ "héllo wörld"
          :filter (lambda (c) (char-alphabetic? c))
          :reverse
          :take 5
          :length
       ) ;$
  =>
  5
) ;check
;; Option 管线：查找 → 变换 → 备选 → 取值
(check ($ '(1 3 5 7)
          :find (> _ 4)
          :map (* _ 10)
          :or-else (option 0)
          :get-or-else -1
       ) ;$
  =>
  50
) ;check

;; === 20. 长链组合：五算子 ===
;; 平方 → 过滤 → 排序 → 取中段 → 拼接
(check ($ '(9 2 7 1 8 3)
          :map (* _ _)
          :filter (<= _ 50)
          :sort-with <
          :drop 1
          :mk-string "+"
       ) ;$
  =>
  "4+9+49"
) ;check
;; 下标配对 → 过滤 → 取首 → 取值
(check
  ($ '("a" "bb" "ccc")
     :zip-with-index
     :filter (lambda (p) (odd? (cadr p)))
     :map car
     :head
  ) ;$
  =>
  "bb"
) ;check
;; 切片 → 去重 → 反转 → 取尾 → 转链表
(check ($ '(1 2 2 3 3 4 5 5)
          :slice 1 7
          :distinct
          :reverse
          :drop-right 1
          :to-list
       ) ;$
  =>
  '(5 4 3)
) ;check
;; 查找 → 转列表 → 映射 → 求和
(check ($ '(2 4 6 8) :find (> _ 5) :to-list :map (* _ 10) :sum) => 60)
;; 向量链：并行映射 → 过滤 → 归约
(check ($ #(1 2 3 4 5) :par-map (* _ 3) :filter (> _ 5) :reduce +) => 42)

(check-report)
