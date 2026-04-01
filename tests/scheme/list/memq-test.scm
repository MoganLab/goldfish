(import (liii check)
        (scheme base)
) ;import

(check-set-mode! 'report-failed)

#|
memq
使用对象标识（eq?比较）在列表中查找指定符号键。此函数是R7RS标准中针对符号的特殊查找优化。

语法
----
(memq symbol lst)

参数
----
symbol :
    要查找的符号键，类型通常为符号(symbol)，但也支持其他可eq?比较的对象类型。

lst : pair?
    要搜索的列表或点对结构。可以是普通列表、关联列表或非空列表结构。
    空列表可作为参数，但返回#f。

返回值
------
list?
    若符号在列表中，返回从第一个匹配项开始的子列表（保留原始内存结构）。
#f
    若未找到指定符号或列表为空。

行为特征
--------
1. 采用eq?进行严格对象标识比较（区别于memv的eqv?比较和member的equal?比较）
2. 专门为符号键设计的最优化查找路径（Symbol Key Desing-Oriented Lookup Pathway）
3. 成功匹配时返回原始列表的尾部子列表，不是复制的新对象
4. 失败时返回#f，无任何系统开销
5. 保持输入输出结构一致性，适用于关联列表操作

边界条件
--------
- 空列表参数：始终返回#f
- 单元素列表边界：成功匹配返回单元素列表
- 重复键处理：返回第一个匹配的尾部子列表
- 非符号类型兼容性：任何可eq?比较对象均支持
- 嵌套结构边界：正确处理深层嵌套和关联列表

性能特征
--------
- 时间复杂度：O(n)，其中n为列表长度，线性遍历
- 空间复杂度：O(1)，无额外内存消耗
- 内存共享：成功时共享原始列表内存结构
- 快速路径：符号键的专用优化比较器
- 递归深度：基于列表长度的线性递归，合理深度安全处理

数据类型兼容性
---------------
- **符号类型**：普通符号、特殊符号、关键字符号
- **数字类型**：整数、浮点数均支持eq?比较
- **字符类型**：所有字符类型统一支持
- **布尔类型**：#t和#f的标准支持
- **过程类型**：内置过程和自定义过程正确识别
- **特殊对象**：点对结构、嵌套列表、构造器结果
- **复合结构**：向量、字节向量、嵌套过程结构

错误处理
--------
wrong-type-arg
    当参数类型不匹配或数量错误时抛出。

应用注意
--------
- 设计用于符号键的优化场景，比member和memv更高效率
- 保持内存结构完全一致，适合连锁操作和状态管理
- 适用于关联列表、谓词列表和环境变量查找
- 返回结构支持继续链式操作（cadr、cddr等）
- 在需要严格对象标识的场景中优先选择memq

限制说明
--------
- eq?比较严格，等价数值例如整数和浮点数值需要完全相同的对象
- 不适用于字符串、数字等需要通过值比较的场景
- 与memv和member形成互补关系，按需求选择
- 不能用于键值对查找（需要assoc等更高级函数）
- 返回的是子列表，不是特定位置的值
|#

;; memq 基本功能测试
(check (memq 'a '(a b c)) => '(a b c))
(check (memq 'b '(a b c)) => '(b c))
(check (memq 'c '(a b c)) => '(c))
(check (memq 'd '(a b c)) => #f)

;; 边界值测试
(check (memq '? '()) => #f)
(check (memq 'only '(only)) => '(only))
(check (memq 'single '(single)) => '(single))
(check (memq 'first '(first second third)) => '(first second third))
(check (memq 'last '(first second last)) => '(last))

;; 符号键查找优化测试
(check (memq 'define '(define lambda if)) => '(define lambda if))
(check (memq 'cond '(if else when unless)) => #f)
(check (memq 'car '(cdr cons car)) => '(car))
(check (memq 'procedure '(symbol list number string)) => #f)

;; 重复键边界测试
(check (memq 'repeat '(a b repeat c repeat d)) => '(repeat c repeat d))
(check (memq 'same '(same same same)) => '(same same same))

;; 布尔值边界测试
(check (memq #t '(#t #f #t)) => '(#t #f #t))
(check (memq #f '(#t #f #t)) => '(#f #t))
(check (memq #t '(#f only)) => #f)

;; 字符键测试
(check (memq #\a '(#\b #\a #\c)) => '(#\a #\c))
(check (memq #\x '(#\a #\b #\c)) => #f)
(check (memq #\newline '(a #\newline b)) => '(#\newline b))

;; 数字边界测试
(check (memq 42 '(1 42 3)) => '(42 3))
(check (memq 0 '(0 1 2)) => '(0 1 2))
(check (memq -1 '(0 1 2)) => #f)
(check (memq 100 '(100 200 300)) => '(100 200 300))

;; 特殊符号字符测试
(check (memq 'λ '(λ α β)) => '(λ α β))
(check (memq '$ '(symbol $ delta)) => '($ delta))

;; 过程对象边界测试
(let ((start car) (next cdr) (last cons))
  (check (memq start (list car cdr start)) => (list car cdr start))
  (check (memq next (list cons list)) => #f)
) ;let

;; 复合结构测试
(let ((test-list '(cons list append)))
  (check (memq 'list test-list) => '(list append))
  (check (memq 'other-symbol test-list) => #f)
) ;let

;; 点对结构测试
(check (memq 'center '(left center right top)) => '(center right top))
(check (memq 'middle '(begin middle end)) => '(middle end))


(check-report)
