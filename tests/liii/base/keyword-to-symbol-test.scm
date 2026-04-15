(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; keyword->symbol - 将关键字转换为符号
;;
;; 语法: (keyword->symbol key)
;; 参数: key - 关键字
;; 返回值: 去掉冒号的普通符号
;;
;; 说明:
;; 将 keyword 转换为普通符号（去掉冒号前缀）


(check (keyword->symbol :baz) => 'baz)
(check (keyword->symbol :hello-world)
  =>
  'hello-world
) ;check
(check (symbol? (keyword->symbol :test))
  =>
  #t
) ;check
(check (keyword? (keyword->symbol :test))
  =>
  #f
) ;check


;; 与 symbol->keyword 的互逆性
(check (equal? (symbol->keyword (keyword->symbol :ghi))
         :ghi
       ) ;equal?
  =>
  #t
) ;check


;; 与 string->keyword 的组合使用
(check (keyword->symbol (string->keyword "def")
       ) ;keyword->symbol
  =>
  'def
) ;check


(check-report)
