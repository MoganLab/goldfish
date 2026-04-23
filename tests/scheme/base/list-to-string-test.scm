(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; list->string
;; 将字符列表转换为字符串。
;;
;; 语法
;; ----
;; (list->string list)
;;
;; 参数
;; ----
;; list : list?
;; 由字符组成的列表。
;;
;; 返回值
;; ------
;; string?
;; 由列表中字符按顺序组成的新字符串。
;;
;; 说明
;; ----
;; 1. 空列表返回空字符串
;; 2. 列表中每个元素必须是字符
(check (list->string '()) => "")
(check (list->string '(#\a)) => "a")
(check (list->string '(#\a #\b #\c))
  =>
  "abc"
) ;check
(check (list->string '(#\1 #\2))
  =>
  "12"
) ;check
(check (string-length (list->string '(#\x #\y #\z))
       ) ;string-length
  =>
  3
) ;check
(check-catch 'wrong-type-arg
  (list->string 'a)
) ;check-catch
(check-catch 'wrong-type-arg
  (list->string '(1 2))
) ;check-catch
(check-catch 'wrong-type-arg
  (list->string '(#\a 1))
) ;check-catch
(check-catch 'wrong-number-of-args
  (list->string)
) ;check-catch
(check-catch 'wrong-number-of-args
  (list->string '(#\a) '(#\b))
) ;check-catch
(check-report)
