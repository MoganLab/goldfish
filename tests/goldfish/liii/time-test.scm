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

#|
time-difference
计算两个时间对象的差值。

语法
----
(time-difference time1 time2)

参数
----
time1 : time?
time2 : time?
两个时间对象，时间类型必须相同。

返回值
-----
time?
返回一个 TIME-DURATION 时间类型的时间对象。

错误处理
--------
wrong-type-arg
当参数不是时间对象或时间类型不匹配时抛出错误。
|#

;; Test time-difference
(let* ((t1 (make-time TIME-UTC 100 5))
       (t2 (make-time TIME-UTC 900000000 3))
       (d  (time-difference t1 t2)))
  (check (time-type d) => TIME-DURATION)
  (check (time-second d) => 1)
  (check (time-nanosecond d) => 100000100))

;; Test negative duration normalization
(let* ((t1 (make-time TIME-UTC 100 5))
       (t2 (make-time TIME-UTC 900000000 5))
       (d  (time-difference t1 t2)))
  (check (time-second d) => -1)
  (check (time-nanosecond d) => 100000100))

;; Test zero difference
(let* ((t1 (make-time TIME-UTC 123456789 42))
       (d  (time-difference t1 t1)))
  (check (time-second d) => 0)
  (check (time-nanosecond d) => 0))

;; Test error conditions
(check-catch 'wrong-type-arg
  (time-difference (make-time TIME-UTC 0 0)
                   (make-time TIME-TAI 0 0)))
(check-catch 'wrong-type-arg
  (time-difference "not-time" (make-time TIME-UTC 0 0)))

#|
add-duration subtract-duration
将时间间隔加到/减去时间对象。

语法
----
(add-duration time1 time-duration)
(subtract-duration time1 time-duration)

参数
----
time1 : time?
time-duration : time? (TIME-DURATION)

返回值
-----
time?
返回一个与 time1 同类型的新时间对象。

错误处理
--------
wrong-type-arg
当参数不是时间对象，或 time-duration 不是 TIME-DURATION 时抛出错误。
|#

;; Test add-duration/subtract-duration
(let* ((t1 (make-time TIME-UTC 100 5))
       (t2 (make-time TIME-UTC 900000000 3))
       (d  (time-difference t1 t2))
       (t3 (add-duration t2 d))
       (t4 (subtract-duration t1 d)))
  (check (time-type t3) => TIME-UTC)
  (check (time-second t3) => 5)
  (check (time-nanosecond t3) => 100)
  (check (time-second t4) => 3)
  (check (time-nanosecond t4) => 900000000))

;; Test negative duration normalization
(let* ((t1 (make-time TIME-UTC 100 5))
       (t2 (make-time TIME-UTC 900000000 5))
       (d  (time-difference t1 t2))
       (t3 (add-duration t2 d)))
  (check (time-second t3) => 5)
  (check (time-nanosecond t3) => 100))

;; Test error conditions
(let ((d (time-difference (make-time TIME-UTC 0 1)
                          (make-time TIME-UTC 0 0))))
  (check-catch 'wrong-type-arg (add-duration "not-time" d))
  (check-catch 'wrong-type-arg (add-duration (make-time TIME-UTC 0 0)
                                             (make-time TIME-UTC 0 0)))
  (check-catch 'wrong-type-arg (subtract-duration "not-time" d))
  (check-catch 'wrong-type-arg (subtract-duration (make-time TIME-UTC 0 0)
                                                  (make-time TIME-UTC 0 0))))

#|
time<=? time<? time=? time>=? time>?
比较两个时间对象的大小。

语法
----
(time<=? time1 time2)
(time<?  time1 time2)
(time=?  time1 time2)
(time>=? time1 time2)
(time>?  time1 time2)

参数
----
time1 : time?
time2 : time?
两个时间对象，时间类型必须相同。

返回值
-----
boolean?
返回比较结果。

错误处理
--------
wrong-type-arg
当参数不是时间对象或时间类型不匹配时抛出错误。
|#

;; Test time comparison
(let* ((t1 (make-time TIME-UTC 100 5))
       (t2 (make-time TIME-UTC 100 5))
       (t3 (make-time TIME-UTC 200 5))
       (t4 (make-time TIME-UTC 0 6)))
  (check (time=? t1 t2) => #t)
  (check (time<? t1 t3) => #t)
  (check (time<=? t1 t3) => #t)
  (check (time>? t4 t3) => #t)
  (check (time>=? t4 t3) => #t)
  (check (time<? t3 t1) => #f)
  (check (time>? t1 t4) => #f))

;; Test comparison error conditions
(check-catch 'wrong-type-arg
  (time<? (make-time TIME-UTC 0 0)
          (make-time TIME-TAI 0 0)))
(check-catch 'wrong-type-arg
  (time=? "not-time" (make-time TIME-UTC 0 0)))

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

#|
date-year-day
获取日期在当年中的序号（1-365/366）。

语法
----
(date-year-day date)

参数
----
date : date?
日期对象。

返回值
-----
integer?
当年中的第几天，1 表示 1 月 1 日。

错误处理
--------
wrong-type-arg
当参数不是日期对象时抛出错误。
value-error
当日期的月份不合法时抛出错误。
|#

;; Test date-year-day
(let ((d1 (make-date 0 0 0 0 1 1 2023 0))   ; non-leap year
      (d2 (make-date 0 0 0 0 1 3 2023 0))
      (d3 (make-date 0 0 0 0 1 3 2024 0))   ; leap year
      (d4 (make-date 0 0 0 0 31 12 2023 0))
      (d5 (make-date 0 0 0 0 31 12 2024 0)))    ; negative year
  (check (date-year-day d1) => 1)
  (check (date-year-day d2) => 60)
  (check (date-year-day d3) => 61)
  (check (date-year-day d4) => 365)
  (check (date-year-day d5) => 366))

;; Test date-year-day error conditions
(check-catch 'wrong-type-arg (date-year-day "not-a-date"))
(check-catch 'value-error (date-year-day (make-date 0 0 0 0 1 0 2023 0)))
(check-catch 'value-error (date-year-day (make-date 0 0 0 0 1 13 2023 0)))

#|
date-week-day
获取日期是星期几（周日=0，周一=1，...）。

语法
----
(date-week-day date)

参数
----
date : date?
日期对象。

返回值
-----
integer?
星期几的编号，范围 0-6。

错误处理
--------
wrong-type-arg
当参数不是日期对象时抛出错误。
|#

;; Test date-week-day
(let ((d1 (make-date 0 0 0 0 1 1 1970 0))   ; 1970-01-01 Thu
      (d2 (make-date 0 0 0 0 25 12 2023 0)) ; 2023-12-25 Mon
      (d3 (make-date 0 0 0 0 29 2 2024 0))) ; 2024-02-29 Thu
  (check (date-week-day d1) => 4)
  (check (date-week-day d2) => 1)
  (check (date-week-day d3) => 4))

;; Test date-week-day error conditions
(check-catch 'wrong-type-arg (date-week-day "not-a-date"))

#|
date-week-number
获取日期在当年的周序号（忽略年初的残周）。

语法
----
(date-week-number date day-of-week-starting-week)

参数
----
date : date?
日期对象。

day-of-week-starting-week : integer?
一周从哪一天开始（周日=0，周一=1，...）。

返回值
-----
integer?
周序号（从 0 开始计数）。

错误处理
--------
wrong-type-arg
当参数不是日期对象时抛出错误。
|#

;; Test date-week-number (ignore first partial week)
(let ((d1 (make-date 0 0 0 0 4 1 1970 0))   ; 1970-01-04 Sun
      (d2 (make-date 0 0 0 0 11 1 1970 0))  ; 1970-01-11 Sun
      (d3 (make-date 0 0 0 0 5 1 1970 0))   ; 1970-01-05 Mon
      (d4 (make-date 0 0 0 0 12 1 1970 0))  ; 1970-01-12 Mon
      (d5 (make-date 0 0 0 0 31 12 2024 0)))
  (check (date-week-number d1 0) => 0)
  (check (date-week-number d2 0) => 1)
  (check (date-week-number d3 1) => 0)
  (check (date-week-number d4 1) => 1)
  (check (date-week-number d5 1) => 52))

;; Test date-week-number error conditions
(check-catch 'wrong-type-arg (date-week-number "not-a-date" 0))

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
;; Time/Date Converters
;; ====================

#|
time-utc->date
将 TIME-UTC 时间对象转换为日期对象。

date->time-utc
将日期对象转换为 TIME-UTC 时间对象。

time-utc->time-tai
将 TIME-UTC 时间对象转换为 TIME-TAI 时间对象。

time-tai->time-utc
将 TIME-TAI 时间对象转换为 TIME-UTC 时间对象。

time-utc->time-monotonic
将 TIME-UTC 时间对象转换为 TIME-MONOTONIC 时间对象。

time-monotonic->time-utc
将 TIME-MONOTONIC 时间对象转换为 TIME-UTC 时间对象。

time-tai->time-monotonic
将 TIME-TAI 时间对象转换为 TIME-MONOTONIC 时间对象。

time-monotonic->time-tai
将 TIME-MONOTONIC 时间对象转换为 TIME-TAI 时间对象。

time-tai->date
将 TIME-TAI 时间对象转换为日期对象。

date->time-tai
将日期对象转换为 TIME-TAI 时间对象。

time-monotonic->date
将 TIME-MONOTONIC 时间对象转换为日期对象。

date->time-monotonic
将日期对象转换为 TIME-MONOTONIC 时间对象。

date->julian-day
将日期对象转换为儒略日。

date->modified-julian-day
将日期对象转换为修正儒略日。

语法
----
(time-utc->date time-utc [tz-offset])
(date->time-utc date)
(time-utc->time-tai time-utc)
(time-tai->time-utc time-tai)
(time-utc->time-monotonic time-utc)
(time-monotonic->time-utc time-monotonic)
(time-tai->time-monotonic time-tai)
(time-monotonic->time-tai time-monotonic)
(time-tai->date time-tai [tz-offset])
(date->time-tai date)
(time-monotonic->date time-monotonic [tz-offset])
(date->time-monotonic date)
(date->julian-day date)
(date->modified-julian-day date)

参数
----
time-utc : time?
必须是 TIME-UTC 类型的时间对象。

time-tai : time?
必须是 TIME-TAI 类型的时间对象。

time-monotonic : time?
必须是 TIME-MONOTONIC 类型的时间对象。

tz-offset : integer? (可选)
时区偏移（秒），默认从 OS 获取本地时区偏移。

date : date?
日期对象。

返回值
-----
time-utc->date : date?
date->time-utc : time?
time-utc->time-tai : time?
time-tai->time-utc : time?
time-utc->time-monotonic : time?
time-monotonic->time-utc : time?
time-tai->time-monotonic : time?
time-monotonic->time-tai : time?
time-tai->date : date?
date->time-tai : time?
time-monotonic->date : date?
date->time-monotonic : time?
date->julian-day : number?
date->modified-julian-day : number?

说明
----
1. time-utc->date 将 UTC 时间按 tz-offset 转换成本地日期。
2. date->time-utc 将本地日期按 date 的 zone-offset 转回 UTC 时间。
3. time-utc->time-tai 根据闰秒表进行转换。
4. time-tai->time-utc 根据闰秒表进行转换。
5. time-utc->time-monotonic 直接复用秒/纳秒进行转换。
6. time-monotonic->time-utc 直接复用秒/纳秒进行转换。
7. time-tai->time-monotonic 先按闰秒表转为 UTC，再复用秒/纳秒构造 TIME-MONOTONIC。
8. time-monotonic->time-tai 先将单调时间视为 UTC 秒数，再按闰秒表转换为 TAI。
9. time-tai->date 先转为 UTC 再按 tz-offset 生成日期。
10. date->time-tai 等价于 date->time-utc 后再转 TAI。
11. time-monotonic->date 以单调时间的秒/纳秒作为 UTC 秒数进行转换。
12. date->time-monotonic 使用 date->time-utc 的秒/纳秒构造 TIME-MONOTONIC。
13. date->julian-day 基于 UTC 时间计算。
14. date->modified-julian-day 基于 UTC 时间计算。

错误处理
--------
wrong-type-arg
当参数类型不正确，或 time-utc/time-tai/time-monotonic 不是对应类型时抛出错误。
|#

;; time-utc->date basic (UTC)
(let* ((t (make-time TIME-UTC 0 0))
       (d (time-utc->date t 0)))
  (check (date-year d) => 1970)
  (check (date-month d) => 1)
  (check (date-day d) => 1)
  (check (date-hour d) => 0)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 0))

;; time-utc->date with positive tz offset (+8)
(let* ((t (make-time TIME-UTC 0 0))
       (d (time-utc->date t 28800)))
  (check (date-year d) => 1970)
  (check (date-month d) => 1)
  (check (date-day d) => 1)
  (check (date-hour d) => 8)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 28800))

;; time-utc->date with positive tz offset (local)
(let* ((t (make-time TIME-UTC 0 0))
       (d (time-utc->date t)))
  (check (date-year d) => 1970)
  (check (date-month d) => 1)
  (check (date-day d) => 1)
  (check (date-hour d) => 8)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 28800))

;; time-utc->date local boundary: 2024-02-28 16:00 UTC -> 2024-02-29 00:00 (UTC+8)
(let* ((t (date->time-utc (make-date 0 0 0 16 28 2 2024 0)))
       (d (time-utc->date t)))
  (check (date-year d) => 2024)
  (check (date-month d) => 2)
  (check (date-day d) => 29)
  (check (date-hour d) => 0)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 28800))

;; time-utc->date local boundary: 2023-02-28 16:00 UTC -> 2023-03-01 00:00 (UTC+8)
(let* ((t (date->time-utc (make-date 0 0 0 16 28 2 2023 0)))
       (d (time-utc->date t)))
  (check (date-year d) => 2023)
  (check (date-month d) => 3)
  (check (date-day d) => 1)
  (check (date-hour d) => 0)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 28800))

;; time-utc->date with negative tz offset (-1 hour)
(let* ((t (make-time TIME-UTC 0 0))
       (d (time-utc->date t -3600)))
  (check (date-year d) => 1969)
  (check (date-month d) => 12)
  (check (date-day d) => 31)
  (check (date-hour d) => 23)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => -3600))

;; time-utc->date before 1970
(let* ((t (make-time TIME-UTC 0 -1))
       (d (time-utc->date t 0)))
  (check (date-year d) => 1969)
  (check (date-month d) => 12)
  (check (date-day d) => 31)
  (check (date-hour d) => 23)
  (check (date-minute d) => 59)
  (check (date-second d) => 59))

;; time-utc->date negative day boundaries
(let* ((t (make-time TIME-UTC 0 -86400))
       (d (time-utc->date t 0)))
  (check (date-year d) => 1969)
  (check (date-month d) => 12)
  (check (date-day d) => 31)
  (check (date-hour d) => 0)
  (check (date-minute d) => 0)
  (check (date-second d) => 0))

(let* ((t (make-time TIME-UTC 0 -86401))
       (d (time-utc->date t 0)))
  (check (date-year d) => 1969)
  (check (date-month d) => 12)
  (check (date-day d) => 30)
  (check (date-hour d) => 23)
  (check (date-minute d) => 59)
  (check (date-second d) => 59))

;; date->time-utc basic
(let* ((d (make-date 0 0 0 8 1 1 1970 28800))
       (t (date->time-utc d)))
  (check (time-type t) => TIME-UTC)
  (check (time-second t) => 0)
  (check (time-nanosecond t) => 0))

;; round-trip date -> time -> date with same tz-offset
(let* ((d1 (make-date 123456789 45 30 14 25 12 2023 28800))
       (t (date->time-utc d1))
       (d2 (time-utc->date t (date-zone-offset d1))))
  (check (date-year d2) => (date-year d1))
  (check (date-month d2) => (date-month d1))
  (check (date-day d2) => (date-day d1))
  (check (date-hour d2) => (date-hour d1))
  (check (date-minute d2) => (date-minute d1))
  (check (date-second d2) => (date-second d1))
  (check (date-nanosecond d2) => (date-nanosecond d1))
  (check (date-zone-offset d2) => (date-zone-offset d1)))

;; 2000-02-29 leap day round-trip (UTC)
(let* ((d1 (make-date 0 0 0 0 29 2 2000 0))
       (t (date->time-utc d1))
       (d2 (time-utc->date t 0)))
  (check (date-year d2) => 2000)
  (check (date-month d2) => 2)
  (check (date-day d2) => 29))

;; additional leap year boundary cases (UTC)
(let* ((d1 (make-date 0 0 0 0 28 2 1900 0)) ; 1900 is not a leap year
       (t (date->time-utc d1))
       (d2 (time-utc->date t 0)))
  (check (date-year d2) => 1900)
  (check (date-month d2) => 2)
  (check (date-day d2) => 28))

(let* ((d1 (make-date 0 0 0 0 1 3 1900 0))
       (t (date->time-utc d1))
       (d2 (time-utc->date t 0)))
  (check (date-year d2) => 1900)
  (check (date-month d2) => 3)
  (check (date-day d2) => 1))

(let* ((d1 (make-date 0 0 0 0 29 2 2004 0)) ; regular leap year
       (t (date->time-utc d1))
       (d2 (time-utc->date t 0)))
  (check (date-year d2) => 2004)
  (check (date-month d2) => 2)
  (check (date-day d2) => 29))

(let* ((d1 (make-date 0 0 0 0 28 2 2100 0)) ; 2100 is not a leap year
       (t (date->time-utc d1))
       (d2 (time-utc->date t 0)))
  (check (date-year d2) => 2100)
  (check (date-month d2) => 2)
  (check (date-day d2) => 28))

(let* ((d1 (make-date 0 0 0 0 1 3 2100 0))
       (t (date->time-utc d1))
       (d2 (time-utc->date t 0)))
  (check (date-year d2) => 2100)
  (check (date-month d2) => 3)
  (check (date-day d2) => 1))

(let* ((d1 (make-date 0 0 0 0 29 2 2400 0)) ; 2400 is a leap year
       (t (date->time-utc d1))
       (d2 (time-utc->date t 0)))
  (check (date-year d2) => 2400)
  (check (date-month d2) => 2)
  (check (date-day d2) => 29))

;; time-utc -> date -> time-utc round-trip cases
(let* ((t1 (make-time TIME-UTC 0 0))
       (d (time-utc->date t1))
       (t2 (date->time-utc d)))
  (check-true (time=? t1 t2)))

(let* ((t1 (make-time TIME-UTC 123456789 98765))
       (d (time-utc->date t1))
       (t2 (date->time-utc d)))
  (check-true (time=? t1 t2)))

(let* ((t1 (make-time TIME-UTC 500000000 -12345))
       (d (time-utc->date t1))
       (t2 (date->time-utc d)))
  (check-true (time=? t1 t2)))

(let* ((t1 (make-time TIME-UTC 0 1704067200))
       (d (time-utc->date t1))
       (t2 (date->time-utc d)))
  (check-true (time=? t1 t2)))

;; converter error conditions
(check-catch 'wrong-type-arg
  (time-utc->date (make-time TIME-TAI 0 0) 0))
(check-catch 'wrong-type-arg
  (time-utc->date (make-time TIME-UTC 0 0) "bad-offset"))
(check-catch 'wrong-type-arg
  (date->time-utc "not-a-date"))

;; time-utc->time-tai / time-tai->time-utc basic
(let* ((t-utc (make-time TIME-UTC 123456789 1483228800))
       (t-tai (time-utc->time-tai t-utc)))
  (check (time-type t-tai) => TIME-TAI)
  (check (time-second t-tai) => 1483228837)
  (check (time-nanosecond t-tai) => 123456789)
  (let ((t-utc2 (time-tai->time-utc t-tai)))
    (check (time-type t-utc2) => TIME-UTC)
    (check (time-second t-utc2) => 1483228800)
    (check (time-nanosecond t-utc2) => 123456789)))

;; Boundary: exactly at leap second effective instant (2017-01-01 00:00:00 UTC)
(let* ((t-utc (make-time TIME-UTC 0 1483228800))
       (t-tai (time-utc->time-tai t-utc)))
  (check (time-type t-tai) => TIME-TAI)
  (check (time-second t-tai) => 1483228837)
  (check (time-nanosecond t-tai) => 0)
  (let ((t-utc2 (time-tai->time-utc t-tai)))
    (check (time-type t-utc2) => TIME-UTC)
    (check (time-second t-utc2) => 1483228800)
    (check (time-nanosecond t-utc2) => 0)))

;; Boundary: just before leap second effective instant
(let* ((t-utc (make-time TIME-UTC 999999999 1483228799))
       (t-tai (time-utc->time-tai t-utc)))
  (check (time-type t-tai) => TIME-TAI)
  (check (time-second t-tai) => 1483228835)
  (check (time-nanosecond t-tai) => 999999999)
  (let ((t-utc2 (time-tai->time-utc t-tai)))
    (check (time-type t-utc2) => TIME-UTC)
    (check (time-second t-utc2) => 1483228799)
    (check (time-nanosecond t-utc2) => 999999999)))

;; Boundary: pre-1972 base offset (UTC epoch)
(let* ((t-utc (make-time TIME-UTC 0 0))
       (t-tai (time-utc->time-tai t-utc)))
  (check (time-type t-tai) => TIME-TAI)
  (check (time-second t-tai) => 10)
  (check (time-nanosecond t-tai) => 0)
  (let ((t-utc2 (time-tai->time-utc t-tai)))
    (check (time-type t-utc2) => TIME-UTC)
    (check (time-second t-utc2) => 0)
    (check (time-nanosecond t-utc2) => 0)))

;; Roundtrip time-utc->time-tai->time-utc
(let* ((t-utc (make-time TIME-UTC 500000000 1435708800)) ; 2015-07-01
       (t-utc2 (time-tai->time-utc (time-utc->time-tai t-utc))))
  (check (time-type t-utc2) => TIME-UTC)
  (check (time-second t-utc2) => 1435708800)
  (check (time-nanosecond t-utc2) => 500000000))

;; Roundtrip time-tai->time-utc->time-tai
(let* ((t-tai (make-time TIME-TAI 250000000 1483228837)) ; 2017-01-01
       (t-tai2 (time-utc->time-tai (time-tai->time-utc t-tai))))
  (check (time-type t-tai2) => TIME-TAI)
  (check (time-second t-tai2) => 1483228837)
  (check (time-nanosecond t-tai2) => 250000000))

;; time-utc->time-tai pre-1972 uses base offset 10
(let* ((t-utc (make-time TIME-UTC 0 0))
       (t-tai (time-utc->time-tai t-utc)))
  (check (time-second t-tai) => 10)
  (check (time-nanosecond t-tai) => 0)
  (check (time-second (time-tai->time-utc t-tai)) => 0))

;; converter error conditions
(check-catch 'wrong-type-arg
  (time-utc->time-tai (make-time TIME-TAI 0 0)))
(check-catch 'wrong-type-arg
  (time-tai->time-utc (make-time TIME-UTC 0 0)))

;; time-utc->time-monotonic / time-monotonic->time-utc basic
(let* ((t-utc (make-time TIME-UTC 123456789 42))
       (t-mon (time-utc->time-monotonic t-utc)))
  (check (time-type t-mon) => TIME-MONOTONIC)
  (check (time-second t-mon) => 42)
  (check (time-nanosecond t-mon) => 123456789)
  (let ((t-utc2 (time-monotonic->time-utc t-mon)))
    (check (time-type t-utc2) => TIME-UTC)
    (check (time-second t-utc2) => 42)
    (check (time-nanosecond t-utc2) => 123456789)))

;; Roundtrip time-utc->time-monotonic->time-utc
(let* ((t-utc (make-time TIME-UTC 500000000 -12345))
       (t-utc2 (time-monotonic->time-utc (time-utc->time-monotonic t-utc))))
  (check (time-type t-utc2) => TIME-UTC)
  (check (time-second t-utc2) => -12345)
  (check (time-nanosecond t-utc2) => 500000000))

;; converter error conditions
(check-catch 'wrong-type-arg
  (time-utc->time-monotonic (make-time TIME-TAI 0 0)))
(check-catch 'wrong-type-arg
  (time-monotonic->time-utc (make-time TIME-UTC 0 0)))

;; time-tai->time-monotonic basic
(let* ((t-tai (make-time TIME-TAI 123456789 1483228837))
       (t-mon (time-tai->time-monotonic t-tai)))
  (check (time-type t-mon) => TIME-MONOTONIC)
  (check (time-second t-mon) => 1483228800)
  (check (time-nanosecond t-mon) => 123456789))

;; time-monotonic->time-tai basic
(let* ((t-mon (make-time TIME-MONOTONIC 123456789 1483228800))
       (t-tai (time-monotonic->time-tai t-mon)))
  (check (time-type t-tai) => TIME-TAI)
  (check (time-second t-tai) => 1483228837)
  (check (time-nanosecond t-tai) => 123456789))

;; Roundtrip time-monotonic->time-tai->time-monotonic
(let* ((t-mon (make-time TIME-MONOTONIC 500000000 -12345))
       (t-mon2 (time-tai->time-monotonic (time-monotonic->time-tai t-mon))))
  (check (time-type t-mon2) => TIME-MONOTONIC)
  (check (time-second t-mon2) => -12345)
  (check (time-nanosecond t-mon2) => 500000000))

;; converter error conditions
(check-catch 'wrong-type-arg
  (time-monotonic->time-tai (make-time TIME-UTC 0 0)))
(check-catch 'wrong-type-arg
  (time-tai->time-monotonic (make-time TIME-UTC 0 0)))

;; time-tai->date basic (UTC epoch)
(let* ((t (make-time TIME-TAI 0 10))
       (d (time-tai->date t 0)))
  (check (date-year d) => 1970)
  (check (date-month d) => 1)
  (check (date-day d) => 1)
  (check (date-hour d) => 0)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 0))

;; time-tai->date with positive tz offset (local)
(let* ((t (make-time TIME-TAI 0 10))
       (d (time-tai->date t)))
  (check (date-year d) => 1970)
  (check (date-month d) => 1)
  (check (date-day d) => 1)
  (check (date-hour d) => 8)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 28800))

;; time-tai->date local boundary: 2024-02-28 16:00 UTC -> 2024-02-29 00:00 (UTC+8)
(let* ((t-utc (date->time-utc (make-date 0 0 0 16 28 2 2024 0)))
       (t-tai (time-utc->time-tai t-utc))
       (d (time-tai->date t-tai)))
  (check (date-year d) => 2024)
  (check (date-month d) => 2)
  (check (date-day d) => 29)
  (check (date-hour d) => 0)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 28800))

;; time-tai->date local boundary: 2023-02-28 16:00 UTC -> 2023-03-01 00:00 (UTC+8)
(let* ((t-utc (date->time-utc (make-date 0 0 0 16 28 2 2023 0)))
       (t-tai (time-utc->time-tai t-utc))
       (d (time-tai->date t-tai)))
  (check (date-year d) => 2023)
  (check (date-month d) => 3)
  (check (date-day d) => 1)
  (check (date-hour d) => 0)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 28800))

;; date->time-tai basic (UTC epoch)
(let* ((d (make-date 0 0 0 0 1 1 1970 0))
       (t (date->time-tai d)))
  (check (time-type t) => TIME-TAI)
  (check (time-second t) => 10)
  (check (time-nanosecond t) => 0))

;; round-trip date -> time-tai -> date with same tz-offset
(let* ((d1 (make-date 123456789 45 30 14 25 12 2023 28800))
       (t (date->time-tai d1))
       (d2 (time-tai->date t (date-zone-offset d1))))
  (check (date-year d2) => (date-year d1))
  (check (date-month d2) => (date-month d1))
  (check (date-day d2) => (date-day d1))
  (check (date-hour d2) => (date-hour d1))
  (check (date-minute d2) => (date-minute d1))
  (check (date-second d2) => (date-second d1))
  (check (date-nanosecond d2) => (date-nanosecond d1))
  (check (date-zone-offset d2) => (date-zone-offset d1)))

;; converter error conditions
(check-catch 'wrong-type-arg
  (time-tai->date (make-time TIME-UTC 0 0) 0))
(check-catch 'wrong-type-arg
  (time-tai->date (make-time TIME-TAI 0 0) "bad-offset"))
(check-catch 'wrong-type-arg
  (date->time-tai "not-a-date"))

;; time-monotonic->date basic (UTC epoch)
(let* ((t (make-time TIME-MONOTONIC 0 0))
       (d (time-monotonic->date t 0)))
  (check (date-year d) => 1970)
  (check (date-month d) => 1)
  (check (date-day d) => 1)
  (check (date-hour d) => 0)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 0))

;; time-monotonic->date with positive tz offset (local)
(let* ((t (make-time TIME-MONOTONIC 0 0))
       (d (time-monotonic->date t)))
  (check (date-year d) => 1970)
  (check (date-month d) => 1)
  (check (date-day d) => 1)
  (check (date-hour d) => 8)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 28800))

;; time-monotonic->date local boundary: 2024-02-28 16:00 UTC -> 2024-02-29 00:00 (UTC+8)
(let* ((t-utc (date->time-utc (make-date 0 0 0 16 28 2 2024 0)))
       (t-mon (time-utc->time-monotonic t-utc))
       (d (time-monotonic->date t-mon)))
  (check (date-year d) => 2024)
  (check (date-month d) => 2)
  (check (date-day d) => 29)
  (check (date-hour d) => 0)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 28800))

;; time-monotonic->date local boundary: 2023-02-28 16:00 UTC -> 2023-03-01 00:00 (UTC+8)
(let* ((t-utc (date->time-utc (make-date 0 0 0 16 28 2 2023 0)))
       (t-mon (time-utc->time-monotonic t-utc))
       (d (time-monotonic->date t-mon)))
  (check (date-year d) => 2023)
  (check (date-month d) => 3)
  (check (date-day d) => 1)
  (check (date-hour d) => 0)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 28800))

;; date->time-monotonic basic (UTC epoch via +8 offset)
(let* ((d (make-date 0 0 0 8 1 1 1970 28800))
       (t (date->time-monotonic d)))
  (check (time-type t) => TIME-MONOTONIC)
  (check (time-second t) => 0)
  (check (time-nanosecond t) => 0))

;; round-trip date -> time-monotonic -> date with same tz-offset
(let* ((d1 (make-date 123456789 45 30 14 25 12 2023 28800))
       (t (date->time-monotonic d1))
       (d2 (time-monotonic->date t (date-zone-offset d1))))
  (check (date-year d2) => (date-year d1))
  (check (date-month d2) => (date-month d1))
  (check (date-day d2) => (date-day d1))
  (check (date-hour d2) => (date-hour d1))
  (check (date-minute d2) => (date-minute d1))
  (check (date-second d2) => (date-second d1))
  (check (date-nanosecond d2) => (date-nanosecond d1))
  (check (date-zone-offset d2) => (date-zone-offset d1)))

;; converter error conditions
(check-catch 'wrong-type-arg
  (time-monotonic->date (make-time TIME-UTC 0 0) 0))
(check-catch 'wrong-type-arg
  (time-monotonic->date (make-time TIME-MONOTONIC 0 0) "bad-offset"))
(check-catch 'wrong-type-arg
  (date->time-monotonic "not-a-date"))

;; date->julian-day / date->modified-julian-day basic (UTC epoch)
(let ((d (make-date 0 0 0 0 1 1 1970 0)))
  (check (date->julian-day d) => 4881175/2)
  (check (date->modified-julian-day d) => 40587))

;; date->julian-day at noon (UTC)
(let ((d (make-date 0 0 0 12 1 1 1970 0)))
  (check (date->julian-day d) => 2440588))

;; date->modified-julian-day next day (UTC)
(let ((d (make-date 0 0 0 0 2 1 1970 0)))
  (check (date->modified-julian-day d) => 40588))

;; converter error conditions
(check-catch 'wrong-type-arg
  (date->julian-day "not-a-date"))
(check-catch 'wrong-type-arg
  (date->modified-julian-day "not-a-date"))

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

#|
string->date
将字符串按模板解析为日期对象。

语法
----
(string->date input-string template-string)

参数
----
input-string : string?
需要解析的输入字符串。

template-string : string?
模板字符串，与 date->string 的格式说明符一致，用 ~ 引导。

返回值
-----
date?
解析得到的日期对象。

说明
----
模板字符串中的 ~ 转义会触发解析规则，输入字符串需整体匹配模板。
常见格式说明符：
  ~Y ~y ~m ~d ~H ~M ~S
  ~A ~a ~B ~b ~p ~z
  ~D ~T ~c ~x ~X

错误处理
--------
wrong-type-arg
当参数不是字符串时抛出错误。
value-error
当输入字符串无法匹配模板时抛出错误。
|#

;; Roundtrip date->string -> string->date
(let* ((d (make-date 0 45 30 14 25 12 2023 28800))
       (fmt "~Y-~m-~d ~H:~M:~S~z")
       (s (date->string d fmt))
       (d2 (string->date s fmt)))
  (check (date-year d2) => 2023)
  (check (date-month d2) => 12)
  (check (date-day d2) => 25)
  (check (date-hour d2) => 14)
  (check (date-minute d2) => 30)
  (check (date-second d2) => 45)
  (check (date-zone-offset d2) => 28800)
  (check (date->string d2 fmt) => s))

(let* ((d (make-date 0 59 59 23 31 12 1999 0))
       (fmt "~A, ~B ~d, ~Y ~I:~M:~S ~p")
       (s (date->string d fmt))
       (d2 (string->date s fmt)))
  (check (date-year d2) => 1999)
  (check (date-month d2) => 12)
  (check (date-day d2) => 31)
  (check (date-hour d2) => 23)
  (check (date-minute d2) => 59)
  (check (date-second d2) => 59)
  (check (date-zone-offset d2) => 0)
  (check (date->string d2 fmt) => s))

;; Test string->date
(let* ((s "2023-12-25 14:30:45")
       (d (string->date s "~Y-~m-~d ~H:~M:~S")))
  (check (date-year d) => 2023)
  (check (date-month d) => 12)
  (check (date-day d) => 25)
  (check (date-hour d) => 14)
  (check (date-minute d) => 30)
  (check (date-second d) => 45))

(let* ((s "Friday, December 31, 1999 11:59:59 PM")
       (d (string->date s "~A, ~B ~d, ~Y ~I:~M:~S ~p")))
  (check (date-year d) => 1999)
  (check (date-month d) => 12)
  (check (date-day d) => 31)
  (check (date-hour d) => 23)
  (check (date-minute d) => 59)
  (check (date-second d) => 59))

(let* ((s "1970-01-01T00:00:00Z")
       (d (string->date s "~4")))
  (check (date-year d) => 1970)
  (check (date-month d) => 1)
  (check (date-day d) => 1)
  (check (date-hour d) => 0)
  (check (date-minute d) => 0)
  (check (date-second d) => 0)
  (check (date-zone-offset d) => 0))

(let* ((s "2023-12-25 14:30:45.123456789")
       (d (string->date s "~Y-~m-~d ~H:~M:~f")))
  (check (date-second d) => 45)
  (check (date-nanosecond d) => 123456789))

;; Test string->date error conditions
(check-catch 'wrong-type-arg (string->date 1 "~Y"))
(check-catch 'wrong-type-arg (string->date "2020" 123))
(check-catch 'value-error (string->date "2020-01-01" "~Y/~m/~d"))

#|
current-date
获取当前日期对象。

语法
----
(current-date [tz-offset])

参数
----
tz-offset : integer? (可选)
时区偏移（秒），默认应为本地时区（当前实现为 0）。

返回值
-----
date?
当前日期对象。

说明
----
1. 当前实现默认使用 UTC（tz-offset=0）。
2. 规范要求默认使用本地时区，后续需要补接口支持。

错误处理
--------
wrong-type-arg
当 tz-offset 不是整数时抛出错误。
|#

;; Test that current date can be converted
(check-true (date? (current-date 0)))
(check-true (string? (date->string (current-date 0))))
(check-true (string? (date->string (current-date 0) "~Y年~m月~d日 ~H时~M分~S秒")))

;; current-date default tz-offset (local)
(let ((d (current-date)))
  (check-true (date? d))
  (check (date-zone-offset d) => 28800))

;; current-date fixed date (Beijing, UTC+8)
;; (let ((d (current-date 28800)))
;;   (check (date-year d) => 2026)
;;   (check (date-month d) => 2)
;;   (check (date-day d) => 19))

;; current-date fixed date (local)
;; (let ((d (current-date)))
;;   (check (date-year d) => 2026)
;;   (check (date-month d) => 2)
;;   (check (date-day d) => 23))

(check-report)
