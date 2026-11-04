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

(import (liii check)
        (liii syntax-case)
        (scheme base))

(check-set-mode! 'report-failed)

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

;; 4. fender 守卫条件分支
(check (syntax-case '(1 2) ()
         ((a b) (> (syntax->datum (syntax a)) 5) 'greater)
         ((a b) 'smaller))
       => 'smaller)

(check (syntax-case '(10 2) ()
         ((a b) (> (syntax->datum (syntax a)) 5) 'greater)
         ((a b) 'smaller))
       => 'greater)

;; 5. 省略号匹配与展开
(check (syntax-case '(1 2 3) ()
         ((a ...) (syntax (a ...))))
       => '(1 2 3))

;; 6. 空列表匹配省略号
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

;; 10. 宏展开生成 (define ...)
(define-syntax def-val-simple
  (lambda (stx)
    (syntax-case stx ()
      ((_ name val)
       (syntax (define name val))))))
(def-val-simple my-v-simple 123)
(check my-v-simple => 123)

;; 11. 宏展开生成 (define ...) 包含 syntactic-closure (Issue #1451)
(define-syntax def-val-datum-to-syntax
  (lambda (stx)
    (syntax-case stx ()
      ((_ name val)
       (datum->syntax (syntax name)
         `(define ,(syntax->datum (syntax name)) ,(syntax->datum (syntax val))))))))
(def-val-datum-to-syntax my-v-1 123)
(check my-v-1 => 123)

(define-syntax def-val-wrapped-target
  (lambda (stx)
    (syntax-case stx ()
      ((_ name val)
       `(define ,(datum->syntax (syntax name) (syntax->datum (syntax name)))
          ,(syntax->datum (syntax val)))))))
(def-val-wrapped-target my-v-2 456)
(check my-v-2 => 456)

(check-report)
