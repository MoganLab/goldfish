(import (liii check) (liii han-zi))


;; number->han-zi
;; 将非负整数按指定风格映射为单个汉字字符。
;;
;; 语法
;; ----
;; (number->han-zi n style)
;;
;; 参数
;; ----
;; n : integer?
;; 要映射的非负整数。
;; style : symbol?
;; 映射风格，可选 'common、'financial、'year。
;;
;; 返回值
;; ----
;; char | #f
;; 对应的汉字字符，若数值或风格不在支持范围内则返回 #f。
;;
;; 示例
;; ----
;; (number->han-zi 0 'common) => #\零
;; (number->han-zi 1 'financial) => #\壹
;; (number->han-zi 10 'year) => #f


;; common 风格
(check (number->han-zi 0 'common) => #\零)
(check (number->han-zi 1 'common) => #\一)
(check (number->han-zi 2 'common) => #\二)
(check (number->han-zi 3 'common) => #\三)
(check (number->han-zi 4 'common) => #\四)
(check (number->han-zi 5 'common) => #\五)
(check (number->han-zi 6 'common) => #\六)
(check (number->han-zi 7 'common) => #\七)
(check (number->han-zi 8 'common) => #\八)
(check (number->han-zi 9 'common) => #\九)
(check (number->han-zi 10 'common) => #\十)
(check (number->han-zi 100 'common) => #\百)
(check (number->han-zi 1000 'common) => #\千)
(check (number->han-zi 10000 'common) => #\万)
(check (number->han-zi 100000000 'common) => #\亿)

;; financial 风格
(check (number->han-zi 0 'financial) => #\零)
(check (number->han-zi 1 'financial) => #\壹)
(check (number->han-zi 2 'financial) => #\贰)
(check (number->han-zi 3 'financial) => #\叁)
(check (number->han-zi 4 'financial) => #\肆)
(check (number->han-zi 5 'financial) => #\伍)
(check (number->han-zi 6 'financial) => #\陆)
(check (number->han-zi 7 'financial) => #\柒)
(check (number->han-zi 8 'financial) => #\捌)
(check (number->han-zi 9 'financial) => #\玖)
(check (number->han-zi 10 'financial) => #\拾)
(check (number->han-zi 100 'financial) => #\佰)
(check (number->han-zi 1000 'financial) => #\仟)
(check (number->han-zi 10000 'financial) => #\万)
(check (number->han-zi 100000000 'financial) => #\亿)

;; year 风格（仅支持 0–9）
(check (number->han-zi 0 'year) => #\〇)
(check (number->han-zi 1 'year) => #\一)
(check (number->han-zi 9 'year) => #\九)
(check-false (number->han-zi 10 'year))
(check-false (number->han-zi 100 'year))
(check-false (number->han-zi 10000 'year))

;; 超出范围
(check-false (number->han-zi -1 'common))
(check-false (number->han-zi 11 'common))
(check-false (number->han-zi 1000000000 'common))
(check-false (number->han-zi 1 'unknown))


(check-report)
