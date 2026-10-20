;; (liii time) 模块函数分类索引
;;
;; time 类似于 Python 的 time 模块，主要用于休眠等基础系统时钟操作。
;; 日期与时间高级处理请使用 (liii date-time)。

(import (liii time)
        (scheme time))

;; 示例：按秒休眠
(sleep 0.001)

;; 示例：读取系统秒级时间戳与 jiffy 精度
(current-second)
(jiffies-per-second)

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/time "sleep"

;; ==== 函数列表 ====
;; sleep - 按秒休眠
