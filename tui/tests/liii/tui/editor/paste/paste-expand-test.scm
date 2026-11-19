;; paste-expand
;; 将文本中包含的所有粘贴占位符标记展开恢复为原始内容。
;;
;; 语法
;; ----
;; (paste-expand reg text)
;;
;; 参数
;; ----
;; reg : paste-registry?
;; 粘贴注册表实例。
;;
;; text : string?
;; 包含占位标记的文本。
;;
;; 返回值
;; ----
;; string?
;; 展开恢复后的完整文本。

(import (scheme base) (liii check) (liii tui editor paste))

(check-set-mode! 'report-failed)

(let* ((reg (make-paste-registry))
       (text (make-string 1965 #\a))
       (marker (paste-registry-add! reg text))
      ) ;
  (check (paste-expand reg marker) => text)
  (check (paste-expand reg (string-append "x" marker "y"))
    =>
    (string-append "x" text "y")
  ) ;check
  (check (paste-expand reg "[paste #9 1 chars]") => "[paste #9 1 chars]")
) ;let*

(check-report)
