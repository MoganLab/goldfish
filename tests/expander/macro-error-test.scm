(import (liii check) (liii string) (liii os))

;; 宏/展开期错误路径回归。
;;
;; goldfish 的展开期错误就是 s7 可捕获异常（syntax-error 只是给
;; (error ...) 加前缀），与 Racket 的 exn:fail:syntax 对应。唯一例外是
;; `gf eval '<坏程序>'` 的 CLI 顶层路径——那里展开先于任何用户代码（含
;; catch），所以错误不可捕获；但展开器的运行时入口（expand-eval / load /
;; load-library!）都会把错误抛进活动 catch，测试用它们即可在进程内断言。

(define (expand-error-message thunk)
  (catch #t
    thunk
    (lambda (tag . info)
      (if (and (pair? info)
               (pair? (car info))
               (string? (caar info)))
        (caar info)
        #f))))

;; ===== 1. eval-transformer：let-syntax 绑定非 transformer =====
(check-catch 'no-catch (expand-eval '(let-syntax ((x 1)) x)))
(check (expand-error-message
         (lambda () (expand-eval '(let-syntax ((x 1)) x))))
       => "eval-transformer: transformer must evaluate to a procedure")

;; ===== 2. eval-when 非法 situation =====
(check-catch 'no-catch (expand-eval '(eval-when (bogus-situation) 1)))
(check (expand-error-message
         (lambda () (expand-eval '(eval-when (bogus-situation) 1))))
       => "eval-when: invalid situation")

;; ===== 3. syntax-rules 无匹配子句 =====
(check-catch 'no-catch
  (expand-eval '(let-syntax ((m (syntax-rules () ((_ x) x)))) (m))))

;; ===== 4. 宏展开产出畸形 core 形式（if 缺分支）=====
(check-catch 'no-catch
  (expand-eval '(let-syntax ((m (syntax-rules () ((_) (if))))) (m))))

;; ===== 5. load 坏文件：展开错误经 load 抛出 =====
(call-with-output-file "/tmp/gf-macro-error-bad.scm"
  (lambda (p) (write '(eval-when (bogus-situation) 1) p) (newline p)))
(check-catch 'no-catch (load "/tmp/gf-macro-error-bad.scm"))

;; ===== 6. load-library! 未知库 =====
(check-catch 'no-catch (load-library! '(no-such-library-here)))
(check (catch #t
         (lambda () (load-library! '(no-such-library-here)))
         (lambda (tag . info)
           (if (and (pair? info) (pair? (car info)) (string? (caar info)))
             (caar info)
             #f)))
       => "import: unknown library")

;; ===== 7. load-library-guard：库体编译期错误报库名（回归 #format-bug）=====
;; 库存在但库体展开失败时，guard 把展开错误转成带库名的消息
;; （"import: failed to load library ..."），而不是二次 format-error 终止进程。
;; 回归点：底层消息无 ~ 占位符（如 "eval-transformer: ..."）曾让 guard 的
;; (apply format (car info)) 抛 format-error，进程死在第二次未捕获错误。
;; 注意：库查找失败（测试 6）直接报 "import: unknown library"，不经过 guard。
(define badlib-dir (os-temp-dir))
(define badlib-sub (string-append badlib-dir "/gf"))
(catch #t (lambda () (mkdir badlib-sub)) (lambda args #f))
(call-with-output-file (string-append badlib-sub "/bad-body-lib.scm")
  (lambda (p)
    (write '(define-library (gf bad-body-lib)
              (import (goldfish))
              (export x)
              (begin (define x (let-syntax ((x 1)) x))))
           p)
    (newline p)))
(if (not (member badlib-dir *load-path*))
  (set! *load-path* (cons badlib-dir *load-path*)))
;; guard 把底层展开错误转成 (error "import: failed to load library ~a: ~a"
;; lib-name detail)：断言进程不二次崩溃（catch 拿到）、消息带库名、且底层
;; 详情被保留。
(let ((r (catch #t
           (lambda () (load-library! '(gf bad-body-lib)))
           (lambda (tag . info) info))))
  (check (pair? r) => #t)
  (check (string? (caar r)) => #t)
  (check (string-contains (caar r) "failed to load library") => #t)
  (check (if (member '(gf bad-body-lib) (cdar r)) #t #f) => #t)
  (check (pair? (cdr (cdar r))) => #t)
  (check (string-contains (car (cdr (cdar r))) "eval-transformer") => #t))

(check-report)
