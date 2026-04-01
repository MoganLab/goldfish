;; (liii queue) 模块函数分类索引
;;
;; queue 提供基于列表的双端队列（list-queue）数据结构，支持高效的两端操作。
;; 它是处理需要先进先出（FIFO）或双端访问场景的理想选择。

;; ==== 常见用法示例 ====
(import (liii queue))

;; 示例1：创建队列并添加元素
(define q (list-queue 1 2 3))
(list-queue-front q) ; => 1
(list-queue-back q)  ; => 3

;; 示例2：队列的两端操作
(list-queue-add-front! q 0)   ; 前端添加
(list-queue-add-back! q 4)    ; 后端添加
(list-queue-remove-front! q)  ; => 0（移除并返回前端元素）

;; 示例3：遍历队列
(list-queue-for-each display q)

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/queue "list-queue-front"
;;   bin/gf doc liii/queue "list-queue-add-back!"

;; ==== 函数分类索引 ====

;; 一、构造函数
;; 用于创建队列的函数
;;   make-list-queue       - 从列表创建队列（支持单参数和双参数形式）
;;   list-queue            - 从任意参数创建队列
;;   list-queue-copy       - 复制队列
;;   list-queue-unfold     - 使用 unfold 创建队列
;;   list-queue-unfold-right - 使用 unfold-right 创建队列

;; 二、谓词
;; 用于判断队列状态的函数
;;   list-queue?           - 检查是否为队列
;;   list-queue-empty?     - 检查队列是否为空

;; 三、访问器
;; 用于访问队列元素的函数
;;   list-queue-front      - 获取队列前端元素
;;   list-queue-back       - 获取队列后端元素
;;   list-queue-list       - 获取队列的列表表示
;;   list-queue-first-last - 获取首尾对（多返回值）

;; 四、修改器
;; 用于修改队列内容的函数（带 ! 后缀表示有副作用）
;;   list-queue-add-front!   - 在前端添加元素
;;   list-queue-add-back!    - 在后端添加元素
;;   list-queue-remove-front! - 从前端移除并返回元素
;;   list-queue-remove-back!  - 从后端移除并返回元素
;;   list-queue-remove-all!   - 移除所有元素，返回列表
;;   list-queue-set-list!     - 设置队列为新列表

;; 五、连接操作
;; 用于连接多个队列的函数
;;   list-queue-append       - 连接多个队列，返回新队列
;;   list-queue-append!      - 连接多个队列，修改第一个队列
;;   list-queue-concatenate  - 连接队列列表

;; 六、映射操作
;; 用于遍历和变换队列的函数
;;   list-queue-map     - 映射函数到队列元素，返回新队列
;;   list-queue-map!    - 映射函数到队列元素，修改原队列
;;   list-queue-for-each - 遍历队列元素
