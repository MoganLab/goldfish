;; (liii time) 模块函数分类索引
;;
;; time 组合了 Goldfish 的基础时间接口与 SRFI-19 日期时间能力。
;; 既可以做简单休眠和时钟读取，也可以处理时间对象、日期格式化与时区转换。

;; ==== 常见用法示例 ====
(import (liii time)
        (srfi srfi-19)
) ;import

;; 示例1：读取系统 jiffy 精度
(jiffies-per-second)

;; 示例2：获取当前时间对象
(time? (current-time)) ; => #t

;; 示例3：构造日期并格式化输出
(date->string (make-date 0 0 0 0 1 1 1970 0) "~Y-~m-~d") ; => "1970-01-01"

;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/time "sleep"
;;   bin/gf doc liii/time "current-time"

;; ==== 函数分类索引 ====

;; 一、基础时钟接口
;; 用于读取当前时钟和休眠的函数
;;   sleep                    - 按秒休眠
;;   current-second           - 获取当前秒时间戳
;;   current-jiffy            - 获取当前 jiffy 计数
;;   jiffies-per-second       - 获取每秒 jiffy 数

;; 二、时间对象
;; 用于创建和修改 time 对象的函数
;;   make-time                - 创建 time 对象
;;   time?                    - 判断对象是否为 time
;;   current-time             - 获取当前 time 对象
;;   time-type                - 获取 time 类型
;;   time-second              - 获取秒部分
;;   time-nanosecond          - 获取纳秒部分
;;   copy-time                - 复制 time 对象
;;   set-time-type!           - 原地设置 time 类型
;;   set-time-second!         - 原地设置秒部分
;;   set-time-nanosecond!     - 原地设置纳秒部分

;; 三、日期对象
;; 用于创建和访问 date 对象的函数
;;   make-date                - 创建 date 对象
;;   date?                    - 判断对象是否为 date
;;   current-date             - 获取当前 date 对象
;;   date-year                - 获取年份
;;   date-month               - 获取月份
;;   date-day                 - 获取日期
;;   date-hour                - 获取小时
;;   date-minute              - 获取分钟
;;   date-second              - 获取秒
;;   date-nanosecond          - 获取纳秒
;;   date-week-day            - 获取周几
;;   date-week-number         - 获取周序号
;;   date-year-day            - 获取年内第几天
;;   date-zone-offset         - 获取时区偏移

;; 四、转换与格式化
;; 用于在不同时间表示之间转换的函数
;;   date->string             - 日期转字符串
;;   string->date             - 字符串转日期
;;   date->time-utc           - 日期转 UTC 时间
;;   time-utc->date           - UTC 时间转日期
;;   time-utc->time-tai       - UTC 时间转 TAI 时间
;;   time-tai->time-utc       - TAI 时间转 UTC 时间
;;   time-utc->time-monotonic - UTC 时间转单调时间
;;   time-monotonic->time-utc - 单调时间转 UTC 时间
;;   date->julian-day         - 日期转儒略日
;;   date->modified-julian-day - 日期转修正儒略日

;; 五、时间计算与比较
;; 用于比较时间对象和计算时间差的函数
;;   add-duration             - 增加持续时间
;;   subtract-duration        - 减去持续时间
;;   time-difference          - 计算两个时间的差
;;   time=?                   - 判断两个时间是否相等
;;   time<?                   - 判断一个时间是否早于另一个
;;   time>?                   - 判断一个时间是否晚于另一个
;;   time<=?                  - 判断一个时间是否早于等于另一个
;;   time>=?                  - 判断一个时间是否晚于等于另一个
;;   time-resolution          - 获取时间分辨率
;;   time-constants           - 获取时间常量
