(import (liii list)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

;; split-at 函数测试
;;
;; 将列表在给定位置拆分为两个部分。
;;
;; 语法
;; ----
;; (split-at list k)
;;
;; 参数
;; ----
;; list : list?
;; 要拆分的列表。
;;
;; k : integer?
;; 拆分位置，必须是非负整数且不超过列表长度。
;;
;; 返回值
;; ------
;; 返回两个值：
;; 1. 列表前k个元素组成的新列表
;; 2. 剩余的元素组成的新列表
;;
;; 说明
;; ----
;; split-at 函数按照 SRFI-1 规范实现，将列表在指定位置分为前后两部分。
;; 该函数是 take 和 drop 函数的组合，等价于同时调用 (take list k) 和 (drop list k)。
;;
;; 对于 proper list，返回两个 new proper lists。
;; 对于 dotted list，拆分在最后可能返回非列表的结束部分。
;;
;; split-at 和 append 互为逆操作：
;; (append (car (split-at lst k)) (cadr (split-at lst k))) = lst
;;
;; 使用场景
;; --------
;; - 列表处理中的分段操作
;; - 实现列表视图和修改分离
;; - 配合 take 和 drop 进行复杂列表转换
;; - 链表算法的实现基础
;;
;; 边界条件
;; --------
;; - 空列表：返回两个空列表
;; - k=0：第一个结果是空列表，第二个是原列表
;; - k=列表长度：第一个结果是原列表，第二个是空列表
;;
;; 错误处理
;; --------
;; - out-of-range：当k超过列表长度或k为负数时
;; - wrong-type-arg：当list不是列表或k不是整数类型时

; 基础功能测试 - 测试split-at正常拆分proper list
(check (list (split-at '(1 2 3 4 5) 3)) => '((1 2 3) (4 5))) ; 正常拆分位置3
(check (list (split-at '(1 2 3 4 5) 0)) => '(() (1 2 3 4 5))) ; 边界：k=0应该返回空列表和完整列表
(check (list (split-at '(1 2 3 4 5) 5)) => '((1 2 3 4 5) ())) ; 边界：k等于列表长度应该返回完整列表和空列表

; 错误处理测试 - 验证不当输入的错误抛出
(check-catch 'value-error (split-at '(1 2 3 4 5) 10)) ; 超出列表长度应该抛value-error
(check-catch 'value-error (split-at '(1 2 3 4 5) -1)) ; 负数应该抛value-error

; dotted list交互测试 - 验证与点结尾列表的交互
(check (list (split-at '(1 2 3 4 . 5) 0)) => '(() (1 2 3 4 . 5))) ; 对于dotted list，k=0的情况
(check (list (split-at '(1 2 3 4 . 5) 3)) => '((1 2 3) (4 . 5))) ; 拆分dotted list到指定位置
(check (list (split-at '(1 2 3 4 . 5) 4)) => '((1 2 3 4) 5)) ; 拆分dotted list到末尾

; dotted list错误处理
(check-catch 'value-error (split-at '(1 2 3 4 . 5) 10)) ; 超出dotted list长度
(check-catch 'value-error (split-at '(1 2 3 4 . 5) -1)) ; 负数对dotted list也不支持

; 空列表边界条件测试
(check (list (split-at '() 0)) => '(() ())) ; 空列表拆分应该返回两个空列表
(check-catch 'value-error (split-at '() 10)) ; 空列表超出范围应该抛value-error
(check-catch 'value-error (split-at '() -1)) ; 空列表负数应该抛value-error

(check-report)
