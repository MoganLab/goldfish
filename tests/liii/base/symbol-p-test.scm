(import (liii check)
        (liii base)
        (liii list)
        (liii case)
        (liii lang)
        (liii error)
        (liii os))

(check-set-mode! 'report-failed)

;; symbol?
;; 判断给定的对象是否为符号(symbol)类型
;;
;; 语法
;; ----
;; (symbol? obj)
;;
;; 参数
;; ----
;; obj : any
;; 任意类型的对象
;;
;; 返回值
;; -----
;; boolean?
;; 如果obj是符号类型则返回#t，否则返回#f
;;
;; 说明
;; ----
;; 符号是Scheme中的基本数据类型之一，用单引号(')前缀表示。
;; 符号在Scheme中通常用作标识符、关键字或枚举值。

(check-true (symbol? 'foo))
(check-true (symbol? (car '(foo bar))))
(check-true (symbol? 'nil))

(check-false (symbol? "bar"))
(check-false (symbol? #f))
(check-false (symbol? '()))
(check-false (symbol? '123))

;; 边界情况测试
(check-true (symbol? '+))
(check-true (symbol? '-))
(check-true (symbol? '*))
(check-true (symbol? '/))
(check-true (symbol? '==))
(check-true (symbol? '=>))

;; 数字开头符号测试
(check-true (symbol? '123abc))
(check-true (symbol? '1a2b3c))

;; 空符号名称测试 (注意：某些scheme系统可能不支持空符号)
(check-true (symbol? (string->symbol "empty-symbol")))

;; 特殊符号测试
(check-true (symbol? 'if))
(check-true (symbol? 'lambda))
(check-true (symbol? 'define))
(check-true (symbol? 'let))
(check-true (symbol? 'begin))

;; 特殊符号格式测试
(check-true (symbol? 'complex_name))
(check-true (symbol? 'symbol_with_underscore))
(check-true (symbol? 'symbol-with-dash))

;; 非符号类型测试
(check-false (symbol? 123))
(check-false (symbol? 123.456))
(check-false (symbol? #\a))
(check-false (symbol? '()))
(check-false (symbol? (list 'a 'b 'c)))
(check-false (symbol? (vector 'a 'b 'c)))

;; 字符串转换测试
(check-true (symbol? (string->symbol "test")))
(check-true (symbol? (string->symbol "complex-symbol-with-numbers")))

(check-report)
