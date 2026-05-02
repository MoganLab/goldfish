(import (liii check) (liii han-zi))


;; han-zi->number
;; 将单个汉字字符映射为非负整数，三种风格一视同仁。
;;
;; 语法
;; ----
;; (han-zi->number ch)
;;
;; 参数
;; ----
;; ch : char?
;; 要映射的汉字字符。
;;
;; 返回值
;; ----
;; integer | #f
;; 对应的数值，若字符不在支持范围内则返回 #f。
;;
;; 示例
;; ----
;; (han-zi->number #\一) => 1
;; (han-zi->number #\壹) => 1
;; (han-zi->number #\兆) => #f


;; 通用风格
(check (han-zi->number #\零) => 0)
(check (han-zi->number #\一) => 1)
(check (han-zi->number #\二) => 2)
(check (han-zi->number #\三) => 3)
(check (han-zi->number #\四) => 4)
(check (han-zi->number #\五) => 5)
(check (han-zi->number #\六) => 6)
(check (han-zi->number #\七) => 7)
(check (han-zi->number #\八) => 8)
(check (han-zi->number #\九) => 9)
(check (han-zi->number #\十) => 10)
(check (han-zi->number #\百) => 100)
(check (han-zi->number #\千) => 1000)
(check (han-zi->number #\万) => 10000)
(check (han-zi->number #\亿) => 100000000)

;; 财务风格
(check (han-zi->number #\壹) => 1)
(check (han-zi->number #\贰) => 2)
(check (han-zi->number #\叁) => 3)
(check (han-zi->number #\肆) => 4)
(check (han-zi->number #\伍) => 5)
(check (han-zi->number #\陆) => 6)
(check (han-zi->number #\柒) => 7)
(check (han-zi->number #\捌) => 8)
(check (han-zi->number #\玖) => 9)
(check (han-zi->number #\拾) => 10)
(check (han-zi->number #\佰) => 100)
(check (han-zi->number #\仟) => 1000)

;; 年份风格
(check (han-zi->number #\〇) => 0)

;; 超出范围
(check-false (han-zi->number #\兆))
(check-false (han-zi->number #\中))
(check-false (han-zi->number #\a))


(check-report)
