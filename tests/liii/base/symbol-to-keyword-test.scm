(import (liii check))
(import (liii base))

(check-set-mode! 'report-failed)

;; symbol->keyword - 将符号转换为关键字
;;
;; 语法: (symbol->keyword sym)
;; 参数: sym - 符号
;; 返回值: 与符号同名但带冒号前缀的关键字
;;
;; 说明:
;; 将普通符号转换为 keyword 类型

(check (equal? (symbol->keyword 'world) :world) => #t)
(check (equal? (symbol->keyword 'foo-bar) :foo-bar) => #t)
(check (keyword? (symbol->keyword 'test)) => #t)

;; 与 keyword->symbol 的互逆性
(check (keyword->symbol (symbol->keyword 'abc)) => 'abc)

(check-report)
