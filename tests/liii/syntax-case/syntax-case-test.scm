(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

;; syntax-case
;; 模式匹配语法对象并支持展开期任意计算过程。
;;
;; 语法
;; ----
;; (syntax-case expr (literal ...) clause ...)
;;
;; clause 形式：
;; (pattern output-expression)
;; (pattern fender output-expression)
;;
;; 参数
;; ----
;; expr : syntax / any
;; 待匹配的目标输入语法对象或表达式。
;;
;; (literal ...) : list of identifier
;; 字面量标识符列表，在匹配时按 free-identifier=? 比较而非作为模式变量绑定。
;;
;; pattern : pattern
;; 匹配模式，支持变量、常数、列表、点对尾部及省略号 ... 重复匹配。
;;
;; fender : boolean-expression (可选)
;; 守卫表达式，仅当匹配成功且 fender 求值为真时该分支才被选中。
;;
;; output-expression : any
;; 匹配成功后求值的表达式，通常包含 (syntax template)。
;;
;; 返回值
;; -----
;; any
;; 首个匹配成功分支输出表达式的求值结果。若无可匹配分支，抛出 syntax-error 错误。

;; 1. 模式变量与符号匹配
(check (syntax-case 'foo ()
         (x (syntax x)))
       => 'foo)

;; 2. 点对与列表解构
(check (syntax-case '(a . b) ()
         ((x . y) (syntax (x y))))
       => '(a b))

;; 3. 字面量匹配
(check (syntax-case '(a . b) (b)
         ((b . y) #f)
         ((x . b) (syntax x)))
       => 'a)

;; 4. 省略号与后置元素解构
(check (syntax-case '(a b c) ()
         ((a ...) (syntax (a ...))))
       => '(a b c))

(check (syntax-case '(a b c) ()
         ((a ... b) (syntax (a ... x b))))
       => '(a b x c))

(check (syntax-case '(a b c . d) ()
         ((a ... b . c) (syntax (a ... x b y c))))
       => '(a b x c y d))

;; 5. 带守卫表达式（fender）的分支分流
(check (syntax-case '(foo 5) ()
         ((_ n) (and (number? (syntax->datum (syntax n)))
                     (even? (syntax->datum (syntax n))))
          'even)
         ((_ n) (and (number? (syntax->datum (syntax n)))
                     (odd? (syntax->datum (syntax n))))
          'odd))
       => 'odd)

;; 6. 空省略号匹配
(check (syntax-case '() ()
         ((a ...) (syntax (a ...))))
       => '())

;; 7. 向量解构
(check (syntax-case '#(1 2 3) ()
         (#(a b c) (syntax #(c b a))))
       => '#(3 2 1))

;; 8. 模式不匹配时抛出 syntax-error
(check-catch 'syntax-error
  (syntax-case '(1 2 3) ()
    ((a b) 'matched)))

;; 9. 基于 define-syntax 与 syntax-case 定义过程式计算宏
(define-syntax calc-add
  (lambda (stx)
    (syntax-case stx ()
      ((_ a b)
       (let ((sum (+ (syntax->datum (syntax a))
                     (syntax->datum (syntax b)))))
         (quasisyntax (list (unsyntax sum))))))))

(check (calc-add 10 20) => '(30))

(check-report)
