(import (liii check) (liii han-zi))


;; han-zi-number?
;; 判断单个字符是否属于本模块管辖范围内的数字汉字。
;;
;; 语法
;; ----
;; (han-zi-number? ch)
;;
;; 参数
;; ----
;; ch : any?
;; 要判断的对象。
;;
;; 返回值
;; ----
;; boolean
;; 若是数字汉字则返回 #t，否则返回 #f。
;;
;; 示例
;; ----
;; (han-zi-number? #\一) => #t
;; (han-zi-number? #\兆) => #f
;; (han-zi-number? #\中) => #f


;; 通用风格数字汉字
(check-true (han-zi-number? #\零))
(check-true (han-zi-number? #\一))
(check-true (han-zi-number? #\二))
(check-true (han-zi-number? #\三))
(check-true (han-zi-number? #\四))
(check-true (han-zi-number? #\五))
(check-true (han-zi-number? #\六))
(check-true (han-zi-number? #\七))
(check-true (han-zi-number? #\八))
(check-true (han-zi-number? #\九))
(check-true (han-zi-number? #\十))
(check-true (han-zi-number? #\百))
(check-true (han-zi-number? #\千))
(check-true (han-zi-number? #\万))
(check-true (han-zi-number? #\亿))

;; 财务风格数字汉字
(check-true (han-zi-number? #\壹))
(check-true (han-zi-number? #\贰))
(check-true (han-zi-number? #\叁))
(check-true (han-zi-number? #\肆))
(check-true (han-zi-number? #\伍))
(check-true (han-zi-number? #\陆))
(check-true (han-zi-number? #\柒))
(check-true (han-zi-number? #\捌))
(check-true (han-zi-number? #\玖))
(check-true (han-zi-number? #\拾))
(check-true (han-zi-number? #\佰))
(check-true (han-zi-number? #\仟))

;; 年份风格
(check-true (han-zi-number? #\〇))

;; 超出范围或不识别
(check-false (han-zi-number? #\兆))
(check-false (han-zi-number? #\中))
(check-false (han-zi-number? #\a))
(check-false (han-zi-number? 1))
(check-false (han-zi-number? "一"))


(check-report)
