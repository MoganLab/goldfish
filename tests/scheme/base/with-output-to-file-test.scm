(import (liii check))
(import (scheme base))
(check-set-mode! 'report-failed)
;; with-output-to-file
;; 临时将输出重定向到文件。
;;
;; 语法
;; ----
;; (with-output-to-file string thunk)
;;
;; 参数
;; ----
;; string : string?
;; 要写入的文件名。
;; thunk : procedure?
;; 无参数过程，在重定向环境中执行。
;;
;; 返回值
;; ------
;; 任意类型
;; thunk 的返回值。
;;
;; 说明
;; ----
;; 1. thunk 执行期间当前输出端口指向该文件
;; 2. 执行结束后恢复原来的输出端口
;; 3. 文件会被覆盖
(let ((result
       (with-output-to-file
         "tests/scheme/base/with-output-to-file-tmp.txt"
         (lambda () (display "hello") 'done)
       ) ;with-output-to-file
      )) ;result
  (check result => 'done)
) ;let

(check-report)
