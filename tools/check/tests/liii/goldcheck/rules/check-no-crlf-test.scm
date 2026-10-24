(import (scheme base)
  (scheme file)
  (liii check)
  (liii path)
  (liii os)
  (liii goldcheck rules)
) ;import

;; check-no-crlf
;; 检查文件换行符是否统一为 LF，发现 CRLF 则报错。
;;
;; 语法
;; ----
;; (check-no-crlf file-path)
;;
;; 参数
;; ----
;; file-path : string
;; 待检查的文件路径。
;;
;; 返回值
;; ------
;; violation-or-#f
;; 违规结构 (list 'no-crlf file-path line)，line 为首个 CRLF 所在行号；
;; 文件全部为 LF（或无换行）时为 #f。
;;
;; 说明
;; ----
;; 测试用 path-write-bytes 二进制写入以精确控制行尾字节，
;; 避免文本端口在 Windows 上把 \n 转写为 \r\n。

(check-set-mode! 'report-failed)

(define (write-test-file name content)
  (let ((tmp-file (path->string (path-join (path-temp-dir) name))))
    (path-write-bytes (path tmp-file) (string->utf8 content))
    tmp-file
  ) ;let
) ;define

;; 1. 纯 LF 文件：通过
(let* ((tmp-file (write-test-file "gf-check-crlf-lf.scm" "(define a 1)\n(define b 2)\n")))
  (check (check-no-crlf tmp-file) => #f)
  (path-unlink tmp-file #t)
) ;let*

;; 2. 纯 CRLF 文件：报错，首个 CRLF 位于第 1 行
(let* ((tmp-file (write-test-file "gf-check-crlf-all.scm" "(define a 1)\r\n(define b 2)\r\n")))
  (let ((viol (check-no-crlf tmp-file)))
    (check (car viol) => 'no-crlf)
    (check (caddr viol) => 1)
  ) ;let
  (path-unlink tmp-file #t)
) ;let*

;; 3. 混合行尾：第 3 行为 CRLF，报错行号 3
(let* ((tmp-file (write-test-file "gf-check-crlf-mixed.scm" "a\nb\nc\r\nd\n")))
  (let ((viol (check-no-crlf tmp-file)))
    (check (car viol) => 'no-crlf)
    (check (caddr viol) => 3)
  ) ;let
  (path-unlink tmp-file #t)
) ;let*

;; 4. 空文件：通过
(let* ((tmp-file (write-test-file "gf-check-crlf-empty.scm" "")))
  (check (check-no-crlf tmp-file) => #f)
  (path-unlink tmp-file #t)
) ;let*

;; 5. 末行无换行符的 LF 文件：通过
(let* ((tmp-file (write-test-file "gf-check-crlf-no-newline.scm" "(define a 1)")))
  (check (check-no-crlf tmp-file) => #f)
  (path-unlink tmp-file #t)
) ;let*

;; 6. 孤立 CR（非 CRLF 序列）：不报错
(let* ((tmp-file (write-test-file "gf-check-crlf-cr-only.scm" "a\rb\n")))
  (check (check-no-crlf tmp-file) => #f)
  (path-unlink tmp-file #t)
) ;let*

(check-report)
