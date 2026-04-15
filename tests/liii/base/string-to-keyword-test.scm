(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; string->keyword - 将字符串转换为关键字
;;
;; 语法: (string->keyword str)
;; 参数: str - 字符串
;; 返回值: 以冒号开头的新关键字
;;
;; 说明:
;; 将普通字符串转换为 keyword 类型，结果以冒号开头


(check (equal? (string->keyword "hello")
         :hello
       ) ;equal?
  =>
  #t
) ;check
(check (equal? (string->keyword "foo-bar")
         :foo-bar
       ) ;equal?
  =>
  #t
) ;check
(check (keyword? (string->keyword "test"))
  =>
  #t
) ;check


(check-report)
