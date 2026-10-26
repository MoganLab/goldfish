;; (liii time) 模块函数分类索引
;;
;; time 类似于 Python 的 time 模块，主要用于休眠等基础系统时钟操作。
;; 日期与时间高级处理请使用 (liii date-time)。

(import (liii time)
        (scheme time))

;; 示例：按秒休眠
(sleep 0.001)

;; 示例：通过差值测量进程和当前线程的 CPU 耗时
(define process-start (process-time))
(define thread-start (thread-time))
(- (process-time) process-start)
(- (thread-time) thread-start)

;; 纳秒版本返回精确整数
(process-time-ns)
(thread-time-ns)

;; 示例：读取系统秒级时间戳与 jiffy 精度
(current-second)
(jiffies-per-second)

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/time "sleep"
;;   bin/gf doc liii/time "process-time"
;;   bin/gf doc liii/time "process-time-ns"
;;   bin/gf doc liii/time "thread-time"
;;   bin/gf doc liii/time "thread-time-ns"

;; ==== 函数列表 ====
;; sleep - 按秒休眠
;; process-time - 当前进程累计 CPU 时间（秒）
;; process-time-ns - 当前进程累计 CPU 时间（纳秒）
;; thread-time - 当前操作系统线程累计 CPU 时间（秒）
;; thread-time-ns - 当前操作系统线程累计 CPU 时间（纳秒）
