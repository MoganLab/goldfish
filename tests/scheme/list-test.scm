;; (scheme base) 列表函数分类索引
;;
;; scheme base 提供 Scheme 标准中的列表操作函数，
;; 包括序对操作、列表构造、选择器、修改器等基础功能。

;; ==== 常见用法示例 ====
(import (scheme base))

;; 示例1：创建和操作列表
;; (cons 1 '(2 3))        ; => (1 2 3)
;; (car '(1 2 3))         ; => 1
;; (cdr '(1 2 3))         ; => (2 3)

;; 示例2：列表查询
;; (null? '())            ; => #t
;; (list? '(1 2 3))       ; => #t
;; (length '(a b c))      ; => 3

;; 示例3：列表组合和拆分
;; (append '(1 2) '(3 4)) ; => (1 2 3 4)
;; (reverse '(1 2 3))     ; => (3 2 1)

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc scheme/base "pair?"
;;   bin/gf doc scheme/base "car"

;; ==== 函数分类索引 ====

;; 一、序对操作
;;   pair?        - 判断是否为序对
;;   car          - 获取序对的 car 部分
;;   cdr          - 获取序对的 cdr 部分
;;   set-car!     - 修改序对的 car 部分
;;   set-cdr!     - 修改序对的 cdr 部分
;;   caar         - 获取 car 的 car
;;   cadr         - 获取 car 的 cdr
;;   cddr         - 获取 cdr 的 cdr

;; 二、列表判断
;;   null?        - 判断是否为空列表
;;   list?        - 判断是否为列表

;; 三、列表构造
;;   make-list    - 创建指定长度的列表
;;   list         - 创建列表
;;   cons         - 构造序对

;; 四、列表查询
;;   length       - 获取列表长度
;;   append       - 连接多个列表
;;   reverse      - 反转列表

;; 五、列表选择
;;   list-tail    - 获取列表尾部
;;   list-ref     - 获取指定索引的元素
;;   list-set!    - 修改指定索引的元素
;;   memq         - 按 eq? 查找元素
;;   memv         - 按 eqv? 查找元素
;;   member       - 按 equal? 查找元素

;; 六、关联列表
;;   assq         - 按 eq? 在关联列表中查找
;;   assv         - 按 eqv? 在关联列表中查找
;;   assoc        - 按 equal? 在关联列表中查找

;; 七、列表复制
;;   list-copy    - 复制列表
