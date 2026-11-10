;; pi-resolve-prompt-input
;; 解析 prompt 输入源（文件/标准输入/字符串）为最终输入文本
;;
;; 语法
;; ----
;; (pi-resolve-prompt-input ...)
;;
;; 参数
;; ----
;; 见各断言场景：本测试按用例组织，覆盖典型入参。
;;
;; 返回值
;; ------
;; 见各断言场景：按被测函数实际返回值断言。

(import (liii check)
  (liii string)
  (liii path)
  (liii pi code core prompt context)
) ;import

(check-set-mode! 'report-failed)

;; pi-resolve-prompt-input
;; 解析直接字符串输入或文件引用输入。

(define REPO_ROOT
  (cond ((file-exists? "xmake.lua") ".")
        ((file-exists? "../../xmake.lua") "../..")
        (else ".")
  ) ;cond
) ;define

(define TEST_FILE
  (path->string (path-join (path REPO_ROOT) (path "tools/pi/tests/fixtures/sample.txt"))
  ) ;path->string
) ;define

(check (pi-resolve-prompt-input "Hello prompt") => "Hello prompt")
(check (pi-resolve-prompt-input #f) => #f)

(let ((content (pi-resolve-prompt-input TEST_FILE)))
  (check-true (string? content))
  (check-true (string-contains? content "hello sample"))
) ;let

(check-report)
