;
; Copyright (C) 2025 The Goldfish Scheme Authors
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
        (liii datetime)
) ;import

(check-set-mode! 'report-failed)

#|
datetime 模块文档

本模块提供日期和时间处理功能，包括日期时间对象、日期对象以及相关计算和格式化功能。

主要组件：
- years: 年份相关操作
- datetime: 日期时间对象
- date: 日期对象
- days: 日期计算

|#

#|
years@leap?
判断指定年份是否为闰年。

语法
----
(years :leap? year)

参数
----
year:integer
待判断的年份。

返回值
-----
boolean
若指定年份为闰年则返回#t，否则返回#f。

错误
----
type-error
若 year 不是整数，则引发类型错误。

额外信息
----
闰年判定规则
能被 4 整除但不能被 100 整除 → 闰年（如 2024）
能被 400 整除 → 闰年（如 2000）
其他情况 → 非闰年（如 2025, 1000）

|#

;; Example for type-error
(check-catch 'type-error (years :leap? 2024.1))

(check-true (years :leap? 2024))
(check-true (years :leap? 2000))

(check-false (years :leap? 2025))
(check-false (years :leap? 1000))


#|
datetime@is-type-of
判断给定对象是否为 datetime 类型的实例。
通过 define-case-class 实现的类型检查方法，无需额外实现。

语法
----
(datetime :is-type-of obj)

参数
----
obj:any
待检查的对象，可以是任何Goldfish Scheme值。

返回值
-----
boolean
若对象obj为datetime类型实例则返回#t，否则返回#f。

额外信息
----
这是通过 define-case-class 宏自动生成的方法，所有案例类都具备类似功能。

|#
(let ((now (datetime :now)))
  (check-true (datetime :is-type-of now))
) ;let

(let ((not-date "2025-01-01"))
  (check-false (datetime :is-type-of not-date))
) ;let

#|
datetime@now
创建一个表示当前系统时间的日期时间对象。
该对象精确到微秒级别，可用于获取当前时间的各个时间分量。

语法
----
(datetime :now)

参数
----
无参数（使用 :now 关键字创建当前时间对象）

返回值
-----
返回一个表示当前日期时间的对象，该对象支持以下字段查询：
'year         : 年份 (>= 2023)
'month        : 月份 (1-12)
'day          : 日期 (1-31)
'hour         : 小时 (0-23)
'minute       : 分钟 (0-59)
'second       : 秒   (0-59)
'micro-second : 微秒 (0-999999)

错误
----
无特定错误（始终返回有效时间对象）

|#

(let ((now (datetime :now)))
  (check-true (datetime :is-type-of now))
  (check-true (>= (now 'year) 2023))  ; Assuming test is run in 2023 or later
  (check-true (<= 1 (now 'month) 12))
  (check-true (<= 1 (now 'day) 31))
  (check-true (<= 0 (now 'hour) 23))
  (check-true (<= 0 (now 'minute) 59))
  (check-true (<= 0 (now 'second) 59))
  (check-true (<= 0 (now 'micro-second) 999999))
) ;let

;; Test microsecond functionality
(let ((dt1 (datetime :now))
      (dt2 (datetime :now)))
  ;; Two close timestamps should have different microsecond values
  (check-true (integer? (dt1 'micro-second)))
  (check-true (integer? (dt2 'micro-second)))
  (check-true (<= 0 (dt1 'micro-second) 999999))
  (check-true (<= 0 (dt2 'micro-second) 999999))
) ;let


#|
datetime%to-string
将 datetime 对象格式化为标准字符串表示。
当微秒为0时，返回 "YYYY-MM-DD HH:MM:SS"，
当微秒非0时，返回 "YYYY-MM-DD HH:MM:SS.MMMMMM" （6位微秒）。

语法
----
(datetime-object :to-string)

参数
----
无参数（直接调用对象方法）。

返回值
-----
返回日期时间字符串：
日期部分：年-月-日
时间部分：时:分:秒
微秒部分：.6位微秒数（不足6位补零）

错误
----
如果调用对象不是有效的 datetime 类型，抛出类型错误。

格式规则
------
|  字段  |   格式化规则   |  示例  |
|--------|----------------|--------|
|   年   |    4位数字     |  2025  |
|  月/日 | 2位数字（补零）|  01,09 |
|时/分/秒| 2位数字（补零）|  00,05 |
|  微秒  | 6位数字（补零）| 000001 |

|#

(check ((datetime :year 2025 :month 1 :day 1) :to-string)
  => "2025-01-01 00:00:00"
) ;check

(check ((datetime :year 2025 :month 1 :day 1 :micro-second 111111) :to-string)
  => "2025-01-01 00:00:00.111111"
) ;check

(check ((datetime :year 2025 :month 1 :day 1 :micro-second 1) :to-string)
  => "2025-01-01 00:00:00.000001"
) ;check

(check ((datetime :year 2025 :month 1 :day 1 :micro-second 999999) :to-string)
  => "2025-01-01 00:00:00.999999"
) ;check


#|
datetime%plus-days
计算当前日期增加/减少指定天数后的新日期对象。

语法
----
(datetime-object :plus-days days)

参数
----
days:integer
整数，表示要增加的天数（正数）或减少的天数（负数）。

返回值
-----
datetime
新的日期时间对象。

错误
----
type-error
若 days 不是整数，则引发类型错误。

额外信息
----
能自动识别闰年（如 2024）与非闰年（如 2023）
跨月时自动调整月份/年份
跨年时自动递增/递减年份
days=0 时返回原日期副本

|#

;; Example for type-error
(check-catch 'type-error ((datetime :year 2024 :month 1 :day 31) :plus-days 1.1))

;; Test plus-days with positive days
(check ((datetime :year 2024 :month 1 :day 1) :plus-days 10) 
  => (datetime :year 2024 :month 1 :day 11)
) ;check

(check ((datetime :year 2024 :month 1 :day 31) :plus-days 1) 
  => (datetime :year 2024 :month 2 :day 1)
) ;check

(check ((datetime :year 2024 :month 1 :day 1) :plus-days 31) 
  => (datetime :year 2024 :month 2 :day 1)
) ;check

(check ((datetime :year 2024 :month 2 :day 28) :plus-days 1) 
  => (datetime :year 2024 :month 2 :day 29) ; 2024 is a leap year
) ;check

(check ((datetime :year 2024 :month 2 :day 29) :plus-days 1) 
  => (datetime :year 2024 :month 3 :day 1)
) ;check

(check ((datetime :year 2023 :month 2 :day 28) :plus-days 1) 
  => (datetime :year 2023 :month 3 :day 1) ; 2023 is not a leap year
) ;check

(check ((datetime :year 2024 :month 12 :day 31) :plus-days 1) 
  => (datetime :year 2025 :month 1 :day 1)
) ;check

(check ((datetime :year 2024 :month 1 :day 1) :plus-days 366) 
  => (datetime :year 2025 :month 1 :day 1) ; 2024 is a leap year
) ;check

;; Test plus-days with negative days
(check ((datetime :year 2024 :month 1 :day 11) :plus-days -10) 
  => (datetime :year 2024 :month 1 :day 1)
) ;check

(check ((datetime :year 2024 :month 2 :day 1) :plus-days -1) 
  => (datetime :year 2024 :month 1 :day 31)
) ;check

(check ((datetime :year 2024 :month 3 :day 1) :plus-days -1) 
  => (datetime :year 2024 :month 2 :day 29)  ; 2024 is a leap year
) ;check

(check ((datetime :year 2023 :month 3 :day 1) :plus-days -1) 
  => (datetime :year 2023 :month 2 :day 28)  ; 2023 is not a leap year
) ;check

(check ((datetime :year 2025 :month 1 :day 1) :plus-days -1) 
  => (datetime :year 2024 :month 12 :day 31)
) ;check

(check ((datetime :year 2025 :month 1 :day 1) :plus-days -365) 
  => (datetime :year 2024 :month 1 :day 2) ; 2024 is a leap year
) ;check

;; Test plus-days with zero
(check ((datetime :year 2024 :month 1 :day 1) :plus-days 0) 
  => (datetime :year 2024 :month 1 :day 1)
) ;check

;; Test preserving time components
(let ((dt (datetime :year 2024 :month 1 :day 1 
            :hour 12 :minute 30 :second 45 :micro-second 123456)))
  (check (dt :plus-days 10) 
    => (datetime :year 2024 :month 1 :day 11 
         :hour 12 :minute 30 :second 45 :micro-second 123456)
  ) ;check
) ;let


#|
datetime%plus-months
计算当前日期增加/减少指定月数后的新日期对象，自动处理月末日期调整。

语法
----
(datetime-object :plus-months months)

参数
----
months:integer
整数，表示要增加的月数（正数）或减少的月数（负数）。

返回值
-----
datetime
新的日期时间对象。

错误
----
type-error
若 months 不是整数，则引发类型错误。

额外信息
----
当原始日期是月末时，结果自动调整为目标月份的最后一天
跨年时自动调整年份
二月天数根据目标年份的闰年状态自动确定
months=0 时返回原日期副本

|#

;; Example for type-error
(check-catch 'type-error ((datetime :year 2024 :month 1 :day 31) :plus-months 1.1))

;; Test plus-months with positive months
(check ((datetime :year 2024 :month 1 :day 15) :plus-months 1) 
  => (datetime :year 2024 :month 2 :day 15)
) ;check

(check ((datetime :year 2024 :month 12 :day 15) :plus-months 1) 
  => (datetime :year 2025 :month 1 :day 15)
) ;check

(check ((datetime :year 2024 :month 1 :day 15) :plus-months 12) 
  => (datetime :year 2025 :month 1 :day 15)
) ;check

(check ((datetime :year 2024 :month 1 :day 15) :plus-months 24) 
  => (datetime :year 2026 :month 1 :day 15)
) ;check

;; Test date adjustment for month end dates
(check ((datetime :year 2024 :month 1 :day 31) :plus-months 1) 
  => (datetime :year 2024 :month 2 :day 29) ; Feb 2024 has 29 days (leap year)
) ;check

(check ((datetime :year 2023 :month 1 :day 31) :plus-months 1) 
  => (datetime :year 2023 :month 2 :day 28) ; Feb 2023 has 28 days (non-leap year)
) ;check

(check ((datetime :year 2024 :month 1 :day 31) :plus-months 2) 
  => (datetime :year 2024 :month 3 :day 31) ; March has 31 days
) ;check

(check ((datetime :year 2024 :month 1 :day 31) :plus-months 3) 
  => (datetime :year 2024 :month 4 :day 30) ; April has 30 days
) ;check

;; Test plus-months with negative months
(check ((datetime :year 2024 :month 3 :day 15) :plus-months -1) 
  => (datetime :year 2024 :month 2 :day 15)
) ;check

(check ((datetime :year 2024 :month 1 :day 15) :plus-months -1) 
  => (datetime :year 2023 :month 12 :day 15)
) ;check

(check ((datetime :year 2024 :month 12 :day 15) :plus-months -12) 
  => (datetime :year 2023 :month 12 :day 15)
) ;check

;; Test date adjustment for month end dates with negative months
(check ((datetime :year 2024 :month 3 :day 31) :plus-months -1) 
  => (datetime :year 2024 :month 2 :day 29) ; Feb 2024 has 29 days (leap year)
) ;check

(check ((datetime :year 2023 :month 3 :day 31) :plus-months -1) 
  => (datetime :year 2023 :month 2 :day 28) ; Feb 2023 has 28 days (non-leap year)
) ;check

;; Test plus-months with zero
(check ((datetime :year 2024 :month 1 :day 15) :plus-months 0) 
  => (datetime :year 2024 :month 1 :day 15)
) ;check

;; Test preserving time components
(let ((dt (datetime :year 2024 :month 1 :day 15 
            :hour 12 :minute 30 :second 45 :micro-second 123456)))
  (check (dt :plus-months 1) 
    => (datetime :year 2024 :month 2 :day 15 
         :hour 12 :minute 30 :second 45 :micro-second 123456)
  ) ;check
) ;let


#|
datetime%plus-years
计算当前日期增加/减少指定年数后的新日期对象。

语法
----
(datetime-object :plus-years years)

参数
----
years:integer
整数，表示要增加的年（正数）或减少的年（负数）。

返回值
-----
datetime
新的日期时间对象。

错误
----
type-error
若 years 不是整数，则引发类型错误。

额外信息
----
能自动识别闰年（如 2024）与非闰年（如 2023）；
当原始日期为闰年2月29日且目标年份非闰年时，日期将调整为2月28日；
years=0 时返回原日期副本。

|#

;; Test plus-years with positive years
(check ((datetime :year 2024 :month 1 :day 15) :plus-years 1) 
  => (datetime :year 2025 :month 1 :day 15)
) ;check

(check ((datetime :year 2024 :month 2 :day 29) :plus-years 1) 
  => (datetime :year 2025 :month 2 :day 28)
) ;check

(check ((datetime :year 2024 :month 2 :day 29) :plus-years 1) 
  => (datetime :year 2025 :month 2 :day 28) ; Feb 29 -> Feb 28 (non-leap year)
) ;check

(check ((datetime :year 2024 :month 2 :day 29) :plus-years 4) 
  => (datetime :year 2028 :month 2 :day 29) ; Feb 29 -> Feb 29 (leap year)
) ;check

(check ((datetime :year 2024 :month 2 :day 29) :plus-years 100) 
  => (datetime :year 2124 :month 2 :day 29) ; 2124 is also a leap year
) ;check

;; Test plus-years with negative years
(check ((datetime :year 2025 :month 1 :day 15) :plus-years -1) 
  => (datetime :year 2024 :month 1 :day 15)
) ;check

(check ((datetime :year 2025 :month 2 :day 28) :plus-years -1) 
  => (datetime :year 2024 :month 2 :day 28)
) ;check

(check ((datetime :year 2025 :month 2 :day 28) :plus-years -5) 
  => (datetime :year 2020 :month 2 :day 28) ; 2020 is a leap year
) ;check

;; Test plus-years with zero
(check ((datetime :year 2024 :month 1 :day 15) :plus-years 0) 
  => (datetime :year 2024 :month 1 :day 15)
) ;check

;; Test preserving time components
(let ((dt (datetime :year 2024 :month 1 :day 15 
            :hour 12 :minute 30 :second 45 :micro-second 123456)))
  (check (dt :plus-years 1) 
    => (datetime :year 2025 :month 1 :day 15 
         :hour 12 :minute 30 :second 45 :micro-second 123456)
  ) ;check
) ;let


#|
date@now
创建一个表示当前系统日期的日期对象。
可用于获取当前日期的年份、月份、日期等字段。

语法
----
(date :now)

参数
----
无参数（使用 :now 关键字创建当前日期对象）

返回值
-----
返回一个表示当前日期的对象，该对象支持以下字段查询：
'year  : 年份 (>= 2023)
'month : 月份 (1-12)
'day   : 日期 (1-31)

错误
----
无特定错误（始终返回有效日期对象）

|#

(check-true (> ((date :now) 'year) 2023))
(let ((today (date :now)))
  (check-true (date :is-type-of today))
  (check-true (>= (today 'year) 2023))
  (check-true (<= 1 (today 'month) 12))
  (check-true (<= 1 (today 'day) 31))
) ;let


#|
date%to-string
将日期转换为格式化的日期字符串，格式为"YYYY-MM-DD"。

语法
----
(date-object :to-string)

参数
----
无参数

返回值
-----
string
格式化的日期字符串。

格式说明
----
- 年份：4位数字
- 月份：2位数字，01-12
- 日期：2位数字，01-31

当数值小于10时，前导补零确保固定长度格式。

错误
----
无特定错误（始终返回格式化的日期字符串）

|#
(check ((date :year 2025 :month 1 :day 1) :to-string)
  => "2025-01-01"
) ;check

(check ((date :year 2025 :month 12 :day 1) :to-string)
  => "2025-12-01"
) ;check

(check ((date :year 2025 :month 3 :day 4) :to-string)
  => "2025-03-04"
) ;check

(check ((date :year 2025 :month 4 :day 12) :to-string)
  => "2025-04-12"
) ;check


#|
datetime%weekday
计算当前日期是星期几。

语法
----
(datetime-object :weekday)

参数
----
无参数

返回值
------
整数 (0-6)
0 表示星期一 (Monday)，1 表示星期二 (Tuesday)，...，6 表示星期日 (Sunday)。

额外信息
------
这是基于 Zeller 公式的变体的计算结果，已调整为周一为起始日。

|#

(check ((datetime :year 2024 :month 1 :day 1) :weekday)  => 0)  ; Monday
(check ((datetime :year 2024 :month 1 :day 2) :weekday)  => 1)  ; Tuesday
(check ((datetime :year 2024 :month 1 :day 7) :weekday)  => 6)  ; Sunday
(check ((datetime :year 2024 :month 1 :day 8) :weekday)  => 0)  ; Monday

(check ((date :year 2024 :month 1 :day 1) :weekday)  => 0)  ; Monday
(check ((date :year 2024 :month 1 :day 7) :weekday)  => 6)  ; Sunday
(check ((date :year 2024 :month 2 :day 29) :weekday)  => 2)  ; Thursday (2024 is leap year)


#|
days@between
计算两个日期之间的天数差异。

语法
----
(days :between start end)

参数
----
start:datetime 或 date
  起始日期，可以是 datetime 对象或 date 对象。
end:datetime 或 date
  结束日期，可以是 datetime 对象或 date 对象。

返回值
-----
long
返回两个日期之间的天数差异，结果为正值或负值。
- 如果 end 在 start 之后，返回正数(表示从开始到结束经过了多少天)
- 如果 end 在 start 之前，返回负数(表示从开始到结束需要倒退多少天)
- 如果两个日期相同，返回 0

错误
----
type-error
如果 start 或 end 不是 datetime 或 date 对象，则抛出类型异常。

额外信息
----
与Java 8 Date Time API中的 DAYS.between(start, end) 类似，计算方法基于日期与1970年1月1日之间的相对天数。

|#

;; Test days between functionality
;; Example for type-error
(check-catch 'type-error (days :between "2025-01-01" (date :year 2024 :month 1 :day 1)))
(check-catch 'type-error (days :between (date :year 2024 :month 1 :day 1) "2025-01-01"))

;; Test with date objects
(check (days :between (date :year 2024 :month 1 :day 1) (date :year 2024 :month 1 :day 15)) => 14)
(check (days :between (date :year 2024 :month 1 :day 15) (date :year 2024 :month 1 :day 1)) => -14)
(check (days :between (date :year 2024 :month 1 :day 15) (date :year 2024 :month 1 :day 15)) => 0)

(check (days :between (date :year 2024 :month 1 :day 31) (date :year 2024 :month 2 :day 1)) => 1)
(check (days :between (date :year 2024 :month 2 :day 28) (date :year 2024 :month 3 :day 1)) => 2) ; leap year
(check (days :between (date :year 2023 :month 2 :day 28) (date :year 2023 :month 3 :day 1)) => 1) ; non-leap year

(check (days :between (date :year 2024 :month 12 :day 31) (date :year 2025 :month 1 :day 1)) => 1)
(check (days :between (date :year 2024 :month 1 :day 1) (date :year 2025 :month 1 :day 1)) => 366) ; leap year
(check (days :between (date :year 2023 :month 1 :day 1) (date :year 2024 :month 1 :day 1)) => 365)   ; non-leap year

;; Test with datetime objects
(check (days :between (datetime :year 2024 :month 1 :day 1 :hour 12 :minute 0 :second 0) 
                      (datetime :year 2024 :month 1 :day 2 :hour 0 :minute 0 :second 0)) => 1)
(check (days :between (datetime :year 2024 :month 1 :day 1 :hour 0 :minute 0 :second 0) 
                      (datetime :year 2024 :month 1 :day 1 :hour 23 :minute 59 :second 59)) => 0) ; same day
(check (days :between (datetime :year 2024 :month 1 :day 15) 
                      (datetime :year 2024 :month 1 :day 1)) => -14)

;; Test mixed datetime and date objects
(check (days :between (date :year 2024 :month 1 :day 1) 
                      (datetime :year 2024 :month 1 :day 15 :hour 12)) => 14)
(check (days :between (datetime :year 2024 :month 1 :day 15 :hour 12) 
                      (date :year 2024 :month 1 :day 1)) => -14)

;; Test large date ranges
(check (days :between (date :year 2000 :month 1 :day 1) (date :year 2100 :month 1 :day 1)) => 36525)
(check (days :between (date :year 2100 :month 1 :day 1) (date :year 2000 :month 1 :day 1)) => -36525)


#|
datetime%to-date
将 datetime 对象转换为 date 对象，保留年月日信息，丢弃时间信息。

语法
----
(datetime-object :to-date)

参数
----
无参数

返回值
-----
date
一个新的 date 对象，包含原 datetime 的年、月、日信息

使用示例
--------
(let ((dt (datetime :year 2024 :month 1 :day 15 :hour 12 :minute 30 :second 45)))
  (dt :to-date)) => (date :year 2024 :month 1 :day 15)

额外信息
----
与Java 8 DateTime API中的 LocalDateTime.toLocalDate() 类似，是datetime的重要转换方法

|#

;; Test datetime%to-date function
(let ((dt (datetime :year 2024 :month 1 :day 15 :hour 12 :minute 30 :second 45 :micro-second 123456)))
  (check ((dt :to-date) 'year) => 2024)
  (check ((dt :to-date) 'month) => 1)
  (check ((dt :to-date) 'day) => 15)
  (check ((dt :to-date) :to-string) => "2024-01-15")
) ;let

;; Test edge cases for datetime%to-date
(let ((dt-edge (datetime :year 2024 :month 2 :day 29 :hour 23 :minute 59 :second 59)))
  (check ((dt-edge :to-date) :to-string) => "2024-02-29") ; leap year Feb 29
) ;let

(let ((dt-min (datetime :year 1 :month 1 :day 1)))
  (check ((dt-min :to-date) :to-string) => "1-01-01")
) ;let

(let ((dt-max (datetime :year 9999 :month 12 :day 31)))
  (check ((dt-max :to-date) :to-string) => "9999-12-31")
) ;let

#|
datetime%format
按照指定的格式字符串格式化日期时间。

语法
----
(datetime-object :format format-string)

参数
----
format-string:string
  格式字符串，支持以下格式符：
  - yyyy: 4位年份
  - MM: 2位月份（01-12）
  - dd: 2位日期（01-31）
  - HH: 2位小时（00-23）
  - mm: 2位分钟（00-59）
  - ss: 2位秒（00-59）
  - SSS: 3位毫秒（000-999）

返回值
-----
string
格式化后的日期时间字符串。

错误
----
value-error
如果格式字符串无效则抛出该异常。

使用示例
--------
((datetime :year 2024 :month 1 :day 15 :hour 14 :minute 30 :second 45 :micro-second 123456) :format "yyyy-MM-dd HH:mm:ss.SSS")
=> "2024-01-15 14:30:45.123"

((datetime :year 2024 :month 1 :day 15) :format "yyyy-MM-dd")
=> "2024-01-15"

只要格式字符串中使用到年月日时分秒毫秒其中一个字段，就算格式正确。

|#

;; Test datetime%format functionality
(let ((dt (datetime :year 2024 :month 1 :day 15 
                    :hour 14 :minute 30 :second 45 :micro-second 123456)))
  (check (dt :format "yyyy-MM-dd HH:mm:ss.SSS") 
    => "2024-01-15 14:30:45.123"
  ) ;check
  
  (check (dt :format "yyyy-MM-dd") 
    => "2024-01-15"
  ) ;check
    
  (check (dt :format "HH:mm:ss") 
    => "14:30:45"
  ) ;check
    
  (check (dt :format "yyyy年MM月dd日 HH时mm分ss秒") 
    => "2024年01月15日 14时30分45秒"
  ) ;check
    
  (check ((datetime :year 2024 :month 3 :day 4 :hour 2 :minute 5 :second 6) :format "dd/MM/yyyy HH:mm")
    => "04/03/2024 02:05"
  ) ;check
) ;let

;; Test format validation - should throw value-error for invalid formats
(let ((dt (datetime :year 2024 :month 1 :day 15)))
  (check-catch 'value-error (dt :format "invalid"))
  (check-catch 'value-error (dt :format "xyz"))
  (check-catch 'value-error (dt :format "abc123"))
  
  ;; Valid formats should not throw error
  (check (dt :format "yyyy") => "2024")
  (check (dt :format "MM") => "01")
  (check (dt :format "dd") => "15")
  (check (dt :format "SSS") => "000")
) ;let

;; Test date%format functionality
(let ((test-date (date :year 2024 :month 1 :day 15)))
  (check (test-date :format "yyyy-MM-dd") => "2024-01-15")
  (check (test-date :format "MM/dd/yyyy") => "01/15/2024")
  (check (test-date :format "dd-MM-yyyy") => "15-01-2024")
  (check (test-date :format "yyyyMMdd") => "20240115")
  (check-catch 'value-error (test-date :format "invalid"))
  (check-catch 'value-error (test-date :format "HH:mm:ss")) ; time formats not supported in date
) ;let


#|
date%to-datetime
将 date 对象转换为 datetime 对象，时间是 (00:00:00.000000)。

语法
----
(date-object :to-datetime)

参数
----
无参数

返回值
-----
datetime
一个新的 datetime 对象，包含原 date 的年月日信息，时分为 0:0:0。

使用示例
--------
((date :year 2024 :month 1 :day 15) :to-datetime)
=> (datetime :year 2024 :month 1 :day 15 :hour 0 :minute 0 :second 0 :micro-second 0)

额外信息
------
与Java 8 DateTime API中的 LocalDate.atStartOfDay() 类似，是date的重要转换方法

|#

;; Test date%to-datetime functionality
(let ((test-date (date :year 2024 :month 1 :day 15))
      (expected-datetime (datetime :year 2024 :month 1 :day 15 
                                   :hour 0 :minute 0 :second 0 :micro-second 0))
      ) ;expected-datetime
  (check (test-date :to-datetime) => expected-datetime)
  
  (let ((result (test-date :to-datetime)))
    (check (result 'year) => 2024)
    (check (result 'month) => 1)
    (check (result 'day) => 15)
    (check (result 'hour) => 0)
    (check (result 'minute) => 0)
    (check (result 'second) => 0)
    (check (result 'micro-second) => 0)
  ) ;let
  
  ;; Test with different dates
  (check ((date :year 2000 :month 2 :day 29) :to-datetime) 
    => (datetime :year 2000 :month 2 :day 29 :hour 0 :minute 0 :second 0 :micro-second 0)
  ) ;check
    
  (check ((date :year 2025 :month 12 :day 31) :to-datetime) 
    => (datetime :year 2025 :month 12 :day 31 :hour 0 :minute 0 :second 0 :micro-second 0)
  ) ;check
) ;let


#|
date%format
按照指定的格式字符串格式化日期。

语法
----
(date-object :format format-string)

参数
----
format-string:string
  格式字符串，支持以下格式符：
  - yyyy: 4位年份
  - MM: 2位月份（01-12）
  - dd: 2位日期（01-31）

返回值
-----
string
格式化后的日期字符串。

错误
----
value-error
如果格式字符串无效则抛出该异常。

使用示例
--------
((date :year 2024 :month 1 :day 15) :format "yyyy-MM-dd")
=> "2024-01-15"

((date :year 2024 :month 1 :day 15) :format "dd/MM/yyyy")
=> "15/01/2024"

|#

;; Test date%format functionality
(let ((test-date (date :year 2024 :month 1 :day 15)))
  (check (test-date :format "yyyy-MM-dd") => "2024-01-15")
  (check (test-date :format "MM/dd/yyyy") => "01/15/2024")
  (check (test-date :format "dd-MM-yyyy") => "15-01-2024")
  (check (test-date :format "yyyyMMdd") => "20240115")
  (check-catch 'value-error (test-date :format "invalid"))
  (check-catch 'value-error (test-date :format "HH:mm:ss")) ; time formats not supported in date
) ;let

#|
date%weekday
计算当前日期是星期几。

语法
----
(date-object :weekday)

参数
----
无参数

返回值
------
整数 (0-6)
0 表示星期一 (Monday)，1 表示星期二 (Tuesday)，...，6 表示星期日 (Sunday)。

使用示例
--------
((date :year 2024 :month 1 :day 1) :weekday) => 0  ; 2024年1月1日是星期一
((date :year 2024 :month 2 :day 29) :weekday) => 2 ; 2024年2月29日是星期四

额外信息
------
这是基于 Zeller 公式的变体的计算结果，已调整为周一为起始日。

|#


(check-report)

