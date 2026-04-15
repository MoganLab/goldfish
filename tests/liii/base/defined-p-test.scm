(import (liii check))
(import (liii base))


(check-set-mode! 'report-failed)


;; defined?
;; 检查符号是否在指定环境中已定义（有绑定值）。
;;
;; 语法
;; ----
;; (defined? symbol)
;; (defined? symbol let)
;; (defined? symbol let ignore-globals)
;;
;; 参数
;; ----
;; symbol : symbol?
;; 要检查的符号（必须加引号）。
;;
;; let : let? 可选，默认为 (curlet)
;; 要在其中查找符号的环境。
;;
;; ignore-globals : boolean? 可选，默认为 #f
;; 如果为 #t，则忽略全局定义，只在指定的 let 中查找。
;;
;; 返回值
;; ------
;; boolean?
;; 如果符号在给定环境中有绑定值，返回 #t；否则返回 #f。
;;
;; 说明
;; ----
;; defined? 用于检查一个符号是否已经被定义（有值）。
;; 注意：与 undefined? 不同，defined? 检查的是符号是否有绑定，
;; 而不是值是否为 #<undefined>。


;; 测试未定义的符号
(check (defined? 'undefined-symbol-xyz)
  =>
  #f
) ;check


;; 测试已定义的符号
(check (let ()
         (define x 42)
         (defined? 'x)
       ) ;let
  =>
  #t
) ;check


;; 测试内置符号
(check (defined? '+) => #t)
(check (defined? 'car) => #t)
(check (defined? 'lambda) => #t)


;; 测试 let 环境中的符号
(check (let ((a 1))
         (defined? 'a)
       ) ;let
  =>
  #t
) ;check


;; 测试在 let 中查找外部符号
(check (let ((a 1))
         (defined? '+)
       ) ;let
  =>
  #t
) ;check


;; 测试指定 let 参数
(check (let ((a 1))
         (defined? 'a (curlet))
       ) ;let
  =>
  #t
) ;check


;; 测试 ignore-globals 参数
(check (let ()
         (defined? '+ (curlet) #t)
       ) ;let
  =>
  #f
) ;check


;; 测试局部变量覆盖
(check (let ((+ 1))
         (defined? '+)
       ) ;let
  =>
  #t
) ;check


(check-report)
