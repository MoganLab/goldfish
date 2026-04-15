(import (liii check) (scheme read))
(check-set-mode! 'report-failed)
;; read
;; 从当前输入端口或指定输入端口中读取一个完整的 Scheme datum。
;;
;; 语法
;; ----
;; (read)
;; (read port)
;;
;; 参数
;; ----
;; port : input-port? 可选
;; 输入端口。省略时，从当前输入端口读取。
;;
;; 返回值
;; ----
;; any
;; 读取到的 datum。若输入已结束，则返回 EOF 对象。
;;
;; 描述
;; ----
;; 1. `read` 会按 Scheme 语法解析文本输入。
;; 2. 可以读取数字、布尔值、字符串、符号、列表等数据。
;; 3. 每次调用只消费一个完整 datum。
;; 4. 输入端口为空时，返回 EOF 对象。
;;
;; 错误处理
;; --------
;; wrong-type-arg
;; 当 `port` 不是输入端口时抛出。
;; read-error
;; 当输入不是合法的 Scheme datum 时抛出。
(check-true (procedure? read))
(check (with-input-from-string "123"
         (lambda () (read))
       ) ;with-input-from-string
  =>
  123
) ;check
(check (with-input-from-string "1 2"
         (lambda () (list (read) (read)))
       ) ;with-input-from-string
  =>
  '(1 2)
) ;check
(check (with-input-from-string "#t"
         (lambda () (read))
       ) ;with-input-from-string
  =>
  #t
) ;check
(check (let ((port (open-input-string "\"goldfish\"")
             ) ;port
            ) ;
         (read port)
       ) ;let
  =>
  "goldfish"
) ;check
(check (let ((port (open-input-string "hello-world"))
            ) ;
         (read port)
       ) ;let
  =>
  'hello-world
) ;check
(check (let ((port (open-input-string "(1 2 (3 4))"))
            ) ;
         (read port)
       ) ;let
  =>
  '(1 2 (3 4))
) ;check
(check (let ((port (open-input-string "()")))
         (read port)
       ) ;let
  =>
  '()
) ;check
(check-true (let ((port (open-input-string "")))
              (eof-object? (read port))
            ) ;let
) ;check-true
(check-catch 'wrong-type-arg (read 123))
(check-report)