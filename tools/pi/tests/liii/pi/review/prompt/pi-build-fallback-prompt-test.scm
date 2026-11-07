;; pi-build-fallback-prompt
;; 构建非结构化降级审查提示词。
;;
;; 语法
;; ----
;; (pi-build-fallback-prompt files)
;;
;; 参数
;; ----
;; files : list?
;; 待审查文件记录列表。
;;
;; 返回值
;; ----
;; string?
;; 组装好的非结构化提示词文本。

(import (liii check)
        (liii string)
        (liii pi review prompt)
) ;import

(check-set-mode! 'report-failed)

(define (file-entry path lang content)
  (list (cons 'path path) (cons 'language lang) (cons 'content content))
) ;define

;; 非结构化回退提示词：文件列表 + 语言清单 + 只读审查指令。
(let* ((files (list (file-entry "src/app.py" "python" "print(1)")))
       (p (pi-build-fallback-prompt files))
      ) ;
  (check-true (string-contains? p "Review the following recently changed files"))
  (check-true (string-contains? p "- src/app.py"))
  (check-true (string-contains? p "### Python"))
  (check-true (string-contains? p "report your findings"))
) ;let*

(check-report)
