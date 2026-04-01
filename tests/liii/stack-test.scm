(import (liii check)
        (liii stack)
) ;import

(check-set-mode! 'report-failed)

;; (liii stack) - 栈数据结构模块
;; 提供后进先出（LIFO）的栈数据结构

;; 构造函数
;; make-stack - 创建空栈或从列表创建栈
;; stack - 从任意参数创建栈

;; 谓词
;; stack? - 检查是否为栈
;; stack-empty? - 检查栈是否为空

;; 访问器
;; stack-top - 获取栈顶元素
;; stack-size - 获取栈中元素个数

;; 修改器
;; stack-push! - 将元素压入栈顶
;; stack-pop! - 从栈顶弹出元素

;; 转换函数
;; stack->list - 将栈转换为列表（从栈顶到栈底）
;; list->stack - 从列表创建栈

;; 映射操作
;; stack-map - 映射函数到栈元素，返回新栈
;; stack-map! - 映射函数到栈元素，修改原栈
;; stack-for-each - 遍历栈元素

;; 复制
;; stack-copy - 复制栈

(display "=== Stack module loaded successfully ===")
(newline)

(check-report)
