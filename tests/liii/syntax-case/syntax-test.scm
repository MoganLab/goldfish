(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; syntax
;; 构建带有词法上下文信息的语法对象模板。
;;
;; 语法
;; ----
;; (syntax template)
;;
;; 参数
;; ----
;; template : template
;; 语法模板，其中的模式变量将被对应绑定的语法值替换，非模式变量则保留输入/定义时的词法绑定。
;;
;; 返回值
;; -----
;; syntax / any
;; 展开并保留词法上下文的语法对象。

;; 1. 基础语法模板与常数
(check (syntax (+ 1 2)) => '(+ 1 2))
(check (syntax "hello") => "hello")
(check (syntax 42) => 42)

;; 2. 模式变量替换
(check (syntax-case '(hello world) ()
         ((a b) (syntax (b a))))
       => '(world hello))

;; 3. 嵌套省略号解构与重排
(check (syntax-case '((a b c) (d e f)) ()
         (((x ... y) ...) (syntax ((x ...) ... y ...))))
       => '((a b) (d e) c f))

;; 4. 向量模板
(check (syntax-case '#(1 2 3) ()
         (#(a b c) (syntax #(c b a))))
       => '#(3 2 1))

(check-report)
