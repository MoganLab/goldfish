(import (liii check)
  (scheme base)
  (scheme eval)
  (srfi srfi-8)
) ;import
(check-set-mode! 'report-failed)
;; environment
;; 创建一个由若干 import set 组成的求值环境。
;;
;; 语法
;; ----
;; (environment import-set ...)
;;
;; 参数
;; ----
;; import-set : import-set
;; 一个或多个 R7RS import set。
;;
;; 返回值
;; ----
;; environment
;; 一个可供 `eval` 使用的环境对象。
;;
;; 描述
;; ----
;; `environment` 根据传入的 import set 构造新的求值环境。该环境中的绑定来自
;; 对应库的导出项，并支持 `only`、`except`、`prefix`、`rename` 这些 import set
;; 变换形式。
;;
;; 示例
;; ----
;; (environment '(scheme base))
;; (environment '(only (scheme base) square))
;; (environment '(rename (scheme base) (square sqr)))
;;
;; 错误处理
;; --------
;; type-error
;; import-set 不是列表时抛出错误。
(check-true (let? (environment)))
(check-true (let? (environment '(scheme base)))
) ;check-true
(check (eval '(square 3)
         (environment '(only (scheme base) square)
         ) ;environment
       ) ;eval
  =>
  9
) ;check
(check (eval '(square 3)
         (environment '(except (scheme base) vector-copy)
         ) ;environment
       ) ;eval
  =>
  9
) ;check
(check (eval '(base-square 3)
         (environment '(prefix (scheme base) base-)
         ) ;environment
       ) ;eval
  =>
  9
) ;check
(check (eval '(sqr 3)
         (environment '(rename (scheme base) (square sqr))
         ) ;environment
       ) ;eval
  =>
  9
) ;check
(check (eval '(receive (a b) (values 1 2) (+ a b))
         (environment '(scheme base)
           '(srfi srfi-8)
         ) ;environment
       ) ;eval
  =>
  3
) ;check
(check-report)
