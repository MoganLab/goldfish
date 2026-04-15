(import (liii check)
        (scheme write)
) ;import

(check-set-mode! 'report-failed)

;; display
;; 将对象按更接近用户阅读的方式写入输出端口。
;;
;; 语法
;; ----
;; (display obj)
;; (display obj port)
;;
;; 参数
;; ----
;; obj : any
;; 要展示的对象。
;;
;; port : output-port? (可选)
;; 输出端口。省略时，写入当前输出端口。
;;
;; 返回值
;; ----
;; unspecified
;; 主要用于副作用输出。
;;
;; 描述
;; ----
;; 1. `display` 更偏向面向用户的展示输出。
;; 2. 对字符串通常不输出双引号。
;; 3. 对符号和数字等对象则输出其展示形式。

(define (capture-output thunk)
  (let ((port (open-output-string)))
    (thunk port)
    (get-output-string port)
  ) ;let
) ;define

(check-true (procedure? display))

(check (capture-output
         (lambda (port)
           (display "goldfish" port)
         ) ;lambda
) ;check
       => "goldfish"
) ;check

(check (capture-output
         (lambda (port)
           (display 42 port)
         ) ;lambda
) ;check
       => "42"
) ;check

(check (capture-output
         (lambda (port)
           (display 'hello port)
         ) ;lambda
) ;check
       => "hello"
) ;check

(check-report)
