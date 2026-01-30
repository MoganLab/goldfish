;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (liii check)
        (liii time)
        (scheme time)
        (liii base)
        (srfi srfi-1)
        (srfi srfi-19))

(check-set-mode! 'report-failed)


#|
sleep
使当前线程暂停执行指定的秒数。

语法
----
(sleep seconds)

参数
----
seconds : number?
暂停的时间长度，以秒为单位。可以是整数或浮点数，表示精确的时间间隔。

返回值
-----
#<unspecified>
返回未指定的值，主要用于其副作用（暂停执行）。

说明
----
1. 暂停当前线程的执行，让出CPU时间给其他线程或进程
2. 实际暂停时间可能受到系统调度精度的影响
3. 对于短时间间隔（如毫秒级），实际暂停时间可能比指定的稍长
4. 参数必须是数值类型，否则会抛出类型错误

错误处理
--------
type-error
当参数不是数值类型时抛出错误。

|#

; Test sleep function
(let ((t1 (current-second)))
  (sleep 1)
  (let ((t2 (current-second)))
    (check (>= (ceiling (- t2 t1)) 1) => #t)))

(let ((t1 (current-second)))
  (sleep 0.5)
  (let ((t2 (current-second)))
    (check (>= (ceiling (- t2 t1)) 0) => #t)))

(check-catch 'type-error (sleep 'not-a-number))


#|
current-second
获取当前时间，以秒为单位。

语法
----
(current-second)

参数
----
无参数

返回值
-----
number?
返回从某个固定时间点（通常是纪元时间）到当前时间的秒数，返回值为浮点数。

说明
----
1. 返回的时间戳通常基于Unix纪元时间（1970-01-01 00:00:00 UTC）
2. 返回值是浮点数类型，支持小数秒精度
3. 精度取决于系统实现，通常为秒级精度
4. 主要用于时间测量和时间戳生成

示例
----
(let ((start (current-second)))
  (sleep 1)
  (let ((end (current-second)))
    (display (format "耗时: ~a 秒" (- end start)))))

|#

(let ((t1 (current-second)))
  (check (number? t1) => #t)
  (check (>= t1 0) => #t))


#|
current-jiffy
获取当前时间的 jiffy 计数。

语法
----
(current-jiffy)

参数
----
无参数

返回值
-----
integer?
返回从某个固定时间点（通常是纪元时间）到当前时间的 jiffy 计数，返回值为整数。

说明
----
1. jiffy 是时间测量单位，1 jiffy = 1/1,000,000 秒（微秒）
2. 返回值是整数类型，提供比秒更精确的时间测量
3. 主要用于高精度时间测量和性能分析
4. 与 current-second 的关系：current-jiffy = (round (* current-second 1000000))

示例
----
(let ((start (current-jiffy)))
  (do-some-work)
  (let ((end (current-jiffy)))
    (display (format "耗时: ~a 微秒" (- end start)))))

|#
(let ((j1 (current-jiffy)))
  (check (integer? j1) => #t)
  (check (>= j1 0) => #t))


#|
jiffies-per-second
获取每秒钟的 jiffy 数量。

语法
----
(jiffies-per-second)

参数
----
无参数

返回值
-----
integer?
返回每秒钟包含的 jiffy 数量，固定值为 1000000。

说明
----
1. 定义 jiffy 与秒之间的换算关系：1 秒 = 1000000 jiffy
2. 返回值是固定的整数，用于时间单位转换
3. 主要用于将 jiffy 时间间隔转换为秒数
4. 与 current-jiffy 配合使用，可以计算精确的时间间隔

示例
----
(let ((start (current-jiffy)))
  (do-some-work)
  (let ((end (current-jiffy)))
    (display (format "耗时: ~a 秒" (/ (- end start) (jiffies-per-second))))))

|#
(check (jiffies-per-second) => 1000000)
(check (integer? (jiffies-per-second)) => #t)
(check (positive? (jiffies-per-second)) => #t)

;; ====================
;; SRFI-19: Time Data Types and Procedures
;; ====================

;; ====================
;; Constants
;; ====================

#|
TIME-DURATION TIME-MONOTONIC TIME-PROCESS TIME-TAI TIME-THREAD TIME-UTC
时间类型常量，用于标识不同类型的时间。

说明
----
这些常量表示不同的时钟类型：
- TIME-DURATION : 时间间隔（无起始点）
- TIME-MONOTONIC: 单调递增的时钟（不受系统时间调整影响）
- TIME-PROCESS  : 进程使用的CPU时间
- TIME-TAI      : 国际原子时
- TIME-THREAD   : 线程使用的CPU时间
- TIME-UTC      : 协调世界时

每个常量对应一个唯一的符号值，用于指定时间对象的类型。
|#

;; Test time constants
(check-true (symbol? TIME-DURATION))
(check-true (symbol? TIME-MONOTONIC))
(check-true (symbol? TIME-PROCESS))
(check-true (symbol? TIME-TAI))
(check-true (symbol? TIME-THREAD))
(check-true (symbol? TIME-UTC))

;; Ensure all constants are distinct
(let ((constants (list TIME-DURATION TIME-MONOTONIC TIME-PROCESS
                       TIME-TAI TIME-THREAD TIME-UTC)))
  (check-true (= (length constants) (length (delete-duplicates constants)))))

;; ====================
;; Time object and accessors
;; ====================

#|
make-time
创建时间对象。

语法
----
(make-time type nanosecond second)

参数
----
type : symbol?
时间类型，必须是时间类型常量之一。

nanosecond : integer?
纳秒部分，必须在 0-999999999 范围内（包含边界）。

second : integer?
秒部分，可以是任意整数。

返回值
-----
time?
一个新的时间对象。

错误处理
--------
wrong-type-arg
当参数类型不正确时抛出错误。
|#

;; Test make-time
(check-true (time? (make-time TIME-UTC 0 0)))
(check-true (time? (make-time TIME-MONOTONIC 500000000 1234567890)))
(check-true (time? (make-time TIME-TAI 999999999 -1234567890)))

;; Test error conditions
(check-catch 'value-error    (make-time 'invalid-type 0 0))
(check-catch 'wrong-type-arg (make-time TIME-UTC 'not-number 0))
(check-catch 'wrong-type-arg (make-time TIME-UTC 0 'not-number))

#|
time?
判断对象是否为时间对象。

语法
----
(time? obj)

参数
----
obj : any?
任意对象。

返回值
-----
boolean?
如果obj是时间对象则返回#t，否则返回#f。
|#

;; Test time?
(check-true  (time? (make-time TIME-UTC 0 0)))
(check-false (time? 123))
(check-false (time? "string"))
(check-false (time? 'symbol))
(check-false (time? #t))
(check-false (time? #(vector)))
(check-false (time? (cons 1 2)))

#|
time-type time-nanosecond time-second
获取时间对象的组成部分。

语法
----
(time-type time)
(time-nanosecond time)
(time-second time)

参数
----
time : time?
时间对象。

返回值
-----
time-type : symbol?
时间类型。

time-nanosecond : integer?
纳秒部分（0-999999999）。

time-second : integer?
秒部分。

错误处理
--------
wrong-type-arg
当参数不是时间对象时抛出错误。
|#

;; Test time accessors
(let ((t1 (make-time TIME-UTC 123456789 987654321))
      (t2 (make-time TIME-MONOTONIC 999999999 0))
      (t3 (make-time TIME-TAI 0 -1234567890)))
  (check (time-type t1) => TIME-UTC)
  (check (time-nanosecond t1) => 123456789)
  (check (time-second t1) => 987654321)

  (check (time-type t2) => TIME-MONOTONIC)
  (check (time-nanosecond t2) => 999999999)
  (check (time-second t2) => 0)

  (check (time-type t3) => TIME-TAI)
  (check (time-nanosecond t3) => 0)
  (check (time-second t3) => -1234567890))

;; Test error conditions
(check-catch 'wrong-type-arg (time-type "not-a-time"))
(check-catch 'wrong-type-arg (time-nanosecond 123))
(check-catch 'wrong-type-arg (time-second 'symbol))

#|
set-time-type! set-time-nanosecond! set-time-second!
设置时间对象的组成部分。

语法
----
(set-time-type! time type)
(set-time-nanosecond! time nanosecond)
(set-time-second! time second)

参数
----
time : time?
要修改的时间对象。

type : symbol?
新的时间类型，必须是时间类型常量之一。

nanosecond : integer?
新的纳秒部分。

second : integer?
新的秒部分。

返回值
-----
any?
返回被设定的新值。

错误处理
--------
wrong-type-arg
当参数类型不正确时抛出错误。
|#

;; Test set-time-*! procedures
(let ((t (make-time TIME-UTC 0 0)))
  (check (set-time-type! t TIME-MONOTONIC)  => TIME-MONOTONIC)
  (check (set-time-nanosecond! t 555555555) => 555555555)
  (check (set-time-second! t 1234567890)    => 1234567890)

  (check (time-type t) => TIME-MONOTONIC)
  (check (time-nanosecond t) => 555555555)
  (check (time-second t) => 1234567890))

;; Test error conditions for set-time-*!
(let ((t (make-time TIME-UTC 0 0)))
  (check-catch 'wrong-type-arg (set-time-type! "not-a-time" TIME-MONOTONIC))
  ;; no check
  (check (set-time-type! t 'invalid-type) => 'invalid-type)
  (check-catch 'wrong-type-arg (set-time-nanosecond! "not-a-time" 0))
  (check-catch 'wrong-type-arg (set-time-second! "not-a-time" 0)))


#|
copy-time
复制时间对象。

语法
----
(copy-time time)

参数
----
time : time?
要复制的时间对象。

返回值
-----
time?
一个新的时间对象，其值与原时间对象相同但独立。

错误处理
--------
wrong-type-arg
当参数不是时间对象时抛出错误。
|#

;; Test copy-time
(let* ((original (make-time TIME-TAI 777777777 888888888))
       (copied (copy-time original)))
  (check-true (time? copied))
  (check (time-type copied) => (time-type original))
  (check (time-nanosecond copied) => (time-nanosecond original))
  (check (time-second copied) => (time-second original))
  ;; Ensure it's a copy, not the same object
  (check-false (eq? original copied))
  ;; Modify original and ensure copy is unchanged
  (set-time-nanosecond! original 999999999)
  (check (time-nanosecond copied) => 777777777))

;; Test error conditions
(check-catch 'wrong-type-arg (copy-time "not-a-time"))

;; ====================
;; Current time and clock resolution
;; ====================

#|
current-time
获取当前时间。

语法
----
(current-time [clock-type])

参数
----
clock-type : symbol? (可选)
时钟类型，默认为 TIME-UTC。必须是时间类型常量之一。

返回值
-----
time?
当前时间的时间对象。

错误处理
--------
wrong-type-arg
当clock-type不是有效的时间类型常量时抛出错误。
|#

;; Test current-time
(check-true (time? (current-time)))
(check-true (time? (current-time TIME-UTC)))
(check-true (time? (current-time TIME-MONOTONIC)))
(check-true (time? (current-time TIME-TAI)))
(check-catch 'wrong-type-arg (time? (current-time TIME-THREAD)))
(check-catch 'wrong-type-arg (time? (current-time TIME-PROCESS)))
(check-catch 'wrong-type-arg (time? (current-time TIME-DURATION)))

;; Check that returned times have correct types
(check (time-type (current-time TIME-UTC))       => TIME-UTC)
(check (time-type (current-time TIME-MONOTONIC)) => TIME-MONOTONIC)
(check (time-type (current-time TIME-TAI))       => TIME-TAI)
(check-catch 'wrong-type-arg (time-type (current-time TIME-THREAD)))
(check-catch 'wrong-type-arg (time-type (current-time TIME-PROCESS)))
(check-catch 'wrong-type-arg (time-type (current-time TIME-DURATION)))

;; Check that nanoseconds are in valid range
(let ((t (current-time)))
  (check-true (>= (time-nanosecond t) 0))
  (check-true (<= (time-nanosecond t) 999999999)))

;; Test monotonic time increases
(let ((t1 (current-time TIME-MONOTONIC))
      (t2 (current-time TIME-MONOTONIC)))
  (check-true (or (> (time-second t2) (time-second t1))
                  (and (= (time-second t2) (time-second t1))
                       (>= (time-nanosecond t2) (time-nanosecond t1))))))

;; Test error conditions
(check-catch 'wrong-type-arg (current-time 'invalid-type))
;
#|
time-resolution
获取时钟分辨率。

语法
----
(time-resolution [clock-type])

参数
----
clock-type : symbol? (可选)
时钟类型，默认为 TIME-UTC。必须是时间类型常量之一。

返回值
-----
integer?
一个纳秒（nanosecond）整数，表示指定时钟的分辨率（精度）。

说明
----
返回值表示该时钟能区分的最小时间间隔。
例如，如果分辨率为1000000，那么纳秒部分可能是1000000的倍数。

错误处理
--------
wrong-type-arg
当clock-type不是有效的时间类型常量时抛出错误。
|#

;; Test time-resolution
(check-true (integer? (time-resolution)))
(check-true (integer? (time-resolution TIME-UTC)))
(check-true (integer? (time-resolution TIME-MONOTONIC)))
(check-true (integer? (time-resolution TIME-TAI)))
(check-catch 'wrong-type-arg (time-resolution TIME-THREAD))
(check-catch 'wrong-type-arg (time-resolution TIME-PROCESS))
(check-catch 'wrong-type-arg (time-resolution TIME-DURATION))

;; Test error conditions
(check-catch 'wrong-type-arg (time-resolution 'invalid-type))

;; ====================
;; Date object and accessors
;; ====================

#|
make-date
创建日期对象。

语法
----
(make-date nanosecond second minute hour day month year zone-offset)

参数
----
nanosecond : integer?

second : integer?

minute : integer?

hour : integer?

day : integer?

month : integer?

year : integer?

zone-offset : integer?

返回值
-----
date?
一个新的日期对象。

错误处理
--------
wrong-type-arg
当参数类型不正确时抛出错误。
|#

;; Test make-date
(check-true (date? (make-date 0 0 0 0 1 1 1970 0)))
(check-true (date? (make-date 999999999 59 59 23 31 12 2023 28800)))
(check-true (date? (make-date 500000000 30 30 12 15 6 2000 -14400)))

;; Test edge cases
(check-true (date? (make-date 0 0 0 0 1 1 0 0)))          ; Year 0
(check-true (date? (make-date 0 0 0 0 1 1 -1000 0)))      ; Year -1000
(check-true (date? (make-date 0 0 0 0 1 1 10000 0)))      ; Year 10000
(check-true (date? (make-date 0 0 0 0 1 1 2023 -64800)))  ; Min zone offset
(check-true (date? (make-date 0 0 0 0 1 1 2023 64800)))   ; Max zone offset

;; Test error conditions
(check-catch 'wrong-type-arg (make-date 'not-number 0 0 0 1 1 1970 0))
;; no range check
(check-true (date? (make-date -1 0 0 0 1 1 1970 0)))
(check-true (date? (make-date 1000000000 0 0 0 1 1 1970 0)))

#|
date?
判断对象是否为日期对象。

语法
----
(date? obj)

参数
----
obj : any?
任意对象。

返回值
-----
boolean?
如果obj是日期对象则返回#t，否则返回#f。
|#

;; Test date?
(check-true  (date? (make-date 0 0 0 0 1 1 1970 0)))
(check-false (date? 123))
(check-false (date? "string"))
(check-false (date? 'symbol))
(check-false (date? #t))
(check-false (date? #(vector)))
(check-false (date? (cons 1 2)))
(check-false (date? (make-time TIME-UTC 0 0)))

#|
date-nanosecond date-second date-minute date-hour
date-day date-month date-year date-zone-offset
获取日期对象的组成部分。

语法
----
(date-nanosecond date)
(date-second date)
(date-minute date)
(date-hour date)
(date-day date)
(date-month date)
(date-year date)
(date-zone-offset date)

参数
----
date : date?
日期对象。

返回值
-----
date-nanosecond : integer?

date-second : integer?

date-minute : integer?

date-hour : integer?

date-day : integer?

date-month : integer?

date-year : integer?

date-zone-offset : integer?

错误处理
--------
wrong-type-arg
当参数不是日期对象时抛出错误。
|#

;; Test date accessors
(let ((d (make-date 123456789 45 30 14 25 12 2023 28800)))
  (check (date-nanosecond d) => 123456789)
  (check (date-second d) => 45)
  (check (date-minute d) => 30)
  (check (date-hour d) => 14)
  (check (date-day d) => 25)
  (check (date-month d) => 12)
  (check (date-year d) => 2023)
  (check (date-zone-offset d) => 28800))

;; Test with different values
(let ((d (make-date 999999999 59 59 23 31 1 2000 -14400)))
  (check (date-nanosecond d) => 999999999)
  (check (date-second d) => 59)
  (check (date-minute d) => 59)
  (check (date-hour d) => 23)
  (check (date-day d) => 31)
  (check (date-month d) => 1)
  (check (date-year d) => 2000)
  (check (date-zone-offset d) => -14400))

;; Test error conditions
(check-catch 'wrong-type-arg (date-nanosecond "not-a-date"))
(check-catch 'wrong-type-arg (date-second 123))
(check-catch 'wrong-type-arg (date-minute 'symbol))
(check-catch 'wrong-type-arg (date-hour #t))
(check-catch 'wrong-type-arg (date-day #(vector)))
(check-catch 'wrong-type-arg (date-month (cons 1 2)))
;; FIXME: strange?
(check-true  (undefined? (date-year (make-time TIME-UTC 0 0))))
(check-catch 'wrong-type-arg (date-zone-offset #f))

;; ====================
;; Date to String/String to Date Converters
;; ====================

#|
date->string
将日期对象转换为字符串。

语法
----
(date->string date [format-string])

参数
----
date : date?
要转换的日期对象。

format-string : string? (可选)
格式字符串，默认为 "~c"（区域设置的日期和时间格式）。

返回值
-----
string?
表示日期的字符串。

说明
----
格式字符串使用类似C的strftime格式说明符，但以~开头。
常见格式说明符：
  ~a : 缩写的星期几名称
  ~A : 完整的星期几名称
  ~b : 缩写的月份名称
  ~B : 完整的月份名称
  ~c : 区域设置的日期和时间表示
  ~d : 月份中的日（01-31）
  ~H : 24小时制的小时（00-23）
  ~I : 12小时制的小时（01-12）
  ~m : 月份（01-12）
  ~M : 分钟（00-59）
  ~p : 区域设置的上午/下午指示符
  ~S : 秒（00-60）
  ~y : 不带世纪的年份（00-99）
  ~Y : 带世纪的年份
  ~z : 时区偏移（+HHMM或-HHMM）
  ~Z : 时区名称或缩写
  ~~ : 字面量的~
参考：https://srfi.schemers.org/srfi-19/srfi-19.html#:~:text=Table%201,-%3A%20DATE
|#

;; Test date->string
(let ((d (make-date 0 0 0 0 1 1 1970 0)))  ; Unix epoch in UTC
  (check (date->string d)            => "Thu Jan 01 00:00:00Z 1970")
  (check (date->string d "~Y-~m-~d") => "1970-01-01")
  (check (date->string d "~H:~M:~S") => "00:00:00"))

;; Test with different dates
(let ((d1 (make-date 500000000 30 15 9 4 7 1776 0))          ; US Independence
      (d2 (make-date 123456789 45 30 14 25 12 2023 28800))   ; Christmas in UTC+8
      (d3 (make-date 999999999 59 59 23 31 12 1999 -18000))) ; Y2K in UTC-5
  (check (date->string d1)                             => "Thu Jul 04 09:15:30Z 1776")
  (check (date->string d2 "~Y-~m-~d ~H:~M:~S")         => "2023-12-25 14:30:45")
  (check (date->string d3 "~A, ~B ~d, ~Y ~I:~M:~S ~p") => "Friday, December 31, 1999 11:59:59 PM"))

;; Test format specifiers
(let ((d (make-date 123456789 45 30 14 25 12 2023 28800)))
  ;; Basic numeric formats
  (check (date->string d "~Y-~m-~d ~H:~M:~S") => "2023-12-25 14:30:45")

  ;; Test literal ~
  (check (date->string d "~~") => "~")

  ;; Test combination of specifiers and literals
  (check (date->string d "Date: ~Y/~m/~d Time: ~H:~M") => "Date: 2023/12/25 Time: 14:30")

  ;; Test CJK
  (check (date->string d "~Y年，第~V週。~H시~M분") => "2023年，第52週。14시30분"))

;; Test error conditions
(let ((d (make-date 0 0 0 0 1 1 1970 0)))
  (check-catch 'wrong-type-arg (date->string "not-a-date"))
  (check-catch 'wrong-type-arg (date->string d 123))  ; format-string not a string
  (check-catch 'wrong-type-arg (date->string d 'symbol)))

;; Test that current date can be converted
(check-catch 'todo (string? (date->string (current-date))))
(check-catch 'todo (string? (date->string (current-date) "~Y年~m月~d日 ~H时~M分~S秒")))

(check-report)
