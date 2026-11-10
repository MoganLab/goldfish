;; pi-build-review-injection
;; 构建自动审查注入文本块，提示模型检查修改过的文件。
;;
;; 语法
;; ----
;; (pi-build-review-injection files)
;;
;; 参数
;; ----
;; files : list?
;; 变更文件信息列表。
;;
;; 返回值
;; ----
;; string? 或 #f
;; 若存在修改文件返回组装好的提示词注入块，否则返回 #f。

(import (liii check) (liii string) (liii pi review auto))

(check-set-mode! 'report-failed)

(define (file-entry path lang)
  (list (cons 'path path) (cons 'language lang))
) ;define

;; 空编辑列表不注入。
(check (pi-build-review-injection '()) => #f)

(let* ((files (list (file-entry "src/a.ts" "typescript") (file-entry "src/b.py" "python"))
       ) ;files
       (block (pi-build-review-injection files))
      ) ;
  (check-true (string-starts? block "\n\n## Code Review\n\nYou edited: src/a.ts, src/b.py.")
  ) ;check-true
  (check-true (string-contains? block "Before proceeding, briefly verify:"))
  (check-true (string-contains? block "### Typescript"))
  (check-true (string-contains? block "### Python"))
  (check-true (string-ends? block "Fix any issues found, then continue."))
) ;let*

;; 多文件同语言时只出现一次清单
(let* ((files (list (file-entry "a.ts" "typescript")
                (file-entry "b.ts" "typescript")
                (file-entry "c.scm" "scheme")
              ) ;list
       ) ;files
       (block (pi-build-review-injection files))
      ) ;
  (check (- (length (string-split block "### Typescript")) 1) => 1)
  (check (- (length (string-split block "### Scheme")) 1) => 1)
  (check-true (string-contains? block "You edited: a.ts, b.ts, c.scm."))
) ;let*

(check-report)
