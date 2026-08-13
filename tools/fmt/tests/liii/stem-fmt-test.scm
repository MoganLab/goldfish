(import (liii check)
  (liii os)
  (liii goldfmt-lang)
  (liii goldfmt-scan)
  (liii goldfmt-format)
  (liii stem-fmt)
  (srfi srfi-13)
) ;import

(check-set-mode! 'report-failed)

(define (resource-file filename)
  (let ((local-path (string-append "tests/resources/" filename))
        (abs-path (string-append "tools/fmt/tests/resources/" filename)))
    (if (access local-path 'R_OK)
        local-path
        abs-path)
  ) ;let
) ;define

;; .stem 后缀派发：注册表能按后缀找到 stem handler
(check (lang-name (lang-for-extension ".stem")) => 'stem)
(check (lang-extensions (lang-for-extension ".stem")) => '(".stem"))

;; 按语言名查询后缀（gf fmt -e stem 走此路径）
(check (extensions-for-lang-name "stem") => '(".stem"))

;; 真实 .stem 文件：stem 模式下格式化后 quote/unquote 保持原样
(define stem-text
  (call-with-stem-mode (lambda ()
                         (format-nodes (scan-file (resource-file "0111_01.stem")))
                       ) ;lambda
  ) ;call-with-stem-mode
) ;define

(check-true (string-contains stem-text "(unquote (arg \"env\"))"))
(check-true (string-contains stem-text "(unquote (merge (arg \"env\") \"-text\"))"))
(check-true (string-contains stem-text "(quote (a b))"))

;; 不能被糖化为 , 或 '
(check-false (string-contains stem-text ",(arg"))
(check-false (string-contains stem-text "'(a b)"))

;; 幂等：格式化结果再次格式化不变
(check (call-with-stem-mode (lambda () (format-string stem-text))) => stem-text)

;; 同一文件在 scm 模式（默认）下会被糖化，证明派发模式确实生效
(define scm-text
  (format-nodes (scan-file (resource-file "0111_01.stem")))
) ;define
(check-true (string-contains scm-text ",(arg \"env\")"))
(check-true (string-contains scm-text "'(a b)"))

(check-report)
