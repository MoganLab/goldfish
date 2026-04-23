(import (liii check))
(import (liii path))
(import (scheme base))
(check-set-mode! 'report-failed)
;; open-output-file
;; 打开文件作为输出端口。
;;
;; 语法
;; ----
;; (open-output-file string)
;;
;; 参数
;; ----
;; string : string?
;; 要创建或覆盖的文件名。
;;
;; 返回值
;; ------
;; output-port?
;; 文件输出端口。
;;
;; 说明
;; ----
;; 1. 如果文件已存在则覆盖
;; 2. 使用后应关闭端口
;; 3. 返回二进制端口
(let ((tmp (path->string (path-join (path-temp-dir) "open-output-file-tmp.txt"))))
  (let ((p (open-output-file tmp)))
    (check (output-port? p) => #t)
    (close-output-port p)
  ) ;let
) ;let

(check-report)
