(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; call-with-port
;; 以端口为参数调用过程，并在过程返回后关闭端口。
;;
;; 语法
;; ----
;; (call-with-port port proc)
;;
;; 参数
;; ----
;; port : port?
;; 输入或输出端口。
;;
;; proc : procedure?
;; 接受端口作为参数的过程。
;;
;; 返回值
;; ------
;; 返回 proc 的返回值。
;;
;; 副作用
;; ------
;; 在 proc 返回后关闭端口。
;; 测试输入端口
(let ((result (call-with-port (open-input-string "hello")
                (lambda (p) (read p))
              ) ;call-with-port
      ) ;result
     ) ;
  (check result => 'hello)
) ;let
;; 测试输出端口
(let ((result (call-with-port (open-output-string)
                (lambda (p)
                  (display "test" p)
                  (get-output-string p)
                ) ;lambda
              ) ;call-with-port
      ) ;result
     ) ;
  (check result => "test")
) ;let
(check-report)
