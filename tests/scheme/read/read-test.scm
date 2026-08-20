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
;;   输入端口。省略时，从当前输入端口读取。
;;
;; 返回值
;; ----
;; any
;;   读取到的 datum。若输入已结束，则返回 EOF 对象。
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
;;   当 `port` 不是输入端口时抛出。
;; read-error
;;   当输入不是合法的 Scheme datum 时抛出。
(check-true (procedure? read))
(check (with-input-from-string "123" (lambda () (read))) => 123)
(check (with-input-from-string "1 2" (lambda () (list (read) (read))))
  =>
  '(1 2)
) ;check
(check (with-input-from-string "#t" (lambda () (read))) => #t)
(check (let ((port (open-input-string "\"goldfish\"")))
         (read port)
       ) ;let
  =>
  "goldfish"
) ;check
(check (let ((port (open-input-string "hello-world")))
         (read port)
       ) ;let
  =>
  'hello-world
) ;check
(check (let ((port (open-input-string "(1 2 (3 4))")))
         (read port)
       ) ;let
  =>
  '(1 2 (3 4))
) ;check
(check (let ((port (open-input-string "()"))) (read port)) => '())
(check-true (let ((port (open-input-string ""))) (eof-object? (read port))))
(check-catch 'wrong-type-arg (read 123))

;; 符号驻留（interning）：重复读取同名符号必须 eq?
(check-true (eq? (with-input-from-string "define" (lambda () (read))) 'define))
(check-true (eq? (with-input-from-string "abc" (lambda () (read)))
                 (with-input-from-string "abc" (lambda () (read)))))
;; 长度恰好为 8 的符号（短符号哈希的边界）
(check-true (eq? (with-input-from-string "abcdefgh" (lambda () (read))) 'abcdefgh))
;; 长度为 9 的符号（超出短符号范围）
(check-true (eq? (with-input-from-string "abcdefghi" (lambda () (read))) 'abcdefghi))
;; 名字前 8 字节相同、后续不同的长符号不能混淆
(check-true (not (eq? (with-input-from-string "abcdefghi1" (lambda () (read)))
                      (with-input-from-string "abcdefghi2" (lambda () (read))))))
;; 单字符符号
(check-true (eq? (with-input-from-string "a" (lambda () (read))) 'a))
;; 短名字数字/符号边界：数字仍按数字解析，"+"/"-"仍是符号
(check (with-input-from-string "42" (lambda () (read))) => 42)
(check-true (eq? (with-input-from-string "+" (lambda () (read))) '+))
(check-true (eq? (with-input-from-string "-" (lambda () (read))) '-))
;; 高重复读取后符号驻留仍然正确（压测符号缓存）
(check-true (let loop ((i 0) (ok #t))
              (if (>= i 100)
                  ok
                  (loop (+ i 1)
                        (and ok (eq? (with-input-from-string "lambda" (lambda () (read))) 'lambda))))))
(check-report)
