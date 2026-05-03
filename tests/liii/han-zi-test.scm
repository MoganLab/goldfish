;; (liii han-zi) 模块函数分类索引
;;
;; han-zi 提供汉字与数值的单字符映射能力。
;; 支持通用、财务、年份三种风格，数值语义边界止于亿（10^8）。


;; ==== 常见用法示例 ====
(import (liii han-zi))


;; 示例1：汉字转数值
(han-zi->number #\一)
(han-zi->number #\壹)


;; 示例2：数值转汉字
(number->han-zi 10 'common)
(number->han-zi 10 'financial)


;; 示例3：判断是否为数字汉字
(han-zi-number? #\亿)
(han-zi-number? #\兆)


;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/han-zi "han-zi->number"
;;   bin/gf doc liii/han-zi "number->han-zi"
;;   bin/gf doc liii/han-zi "han-zi-number?"


;; ==== 函数分类索引 ====


;; 一、汉字与数值互转
;; 用于单字符级别的汉字与数值映射
;;   han-zi->number          - 将汉字字符映射为数值
;;   number->han-zi          - 将数值按风格映射为汉字字符


;; 二、判定函数
;; 用于判断字符是否属于本模块管辖范围的数字汉字
;;   han-zi-number?           - 判断是否为数字汉字
