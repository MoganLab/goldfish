(import (liii check)
        (liii string)
        (liii pi args)
        (liii pi compass args)
) ;import

(check-set-mode! 'report-failed)

;; --codemap / --tour / --refresh / --codemap-file / --no-codemap-inject 解析。
(check (pi-args-ref (parse-pi-args '("--codemap")) 'codemap) => #t)
(check (pi-args-ref (parse-pi-args '("--tour")) 'tour) => #t)
(check (pi-args-ref (parse-pi-args '("--tour")) 'tour-topic) => #f)
(check (pi-args-ref (parse-pi-args '("--tour" "src")) 'tour) => "src")
(check (pi-args-ref (parse-pi-args '("--tour" "src")) 'messages) => '())
(check (pi-args-ref (parse-pi-args '("--tour" "src" "hello")) 'messages)
  =>
  '("hello")
) ;check
(check (pi-args-ref (parse-pi-args '("--tour" "--no-tools")) 'tour) => #t)
(check (pi-args-ref (parse-pi-args '("--tour" "--no-tools")) 'tour-topic) => #f)
(check (pi-args-ref (parse-pi-args '("--tour" "--no-tools")) 'no-tools) => #t)

(check (pi-args-ref (parse-pi-args '("--codemap" "--refresh")) 'refresh) => #t)
(check (pi-args-ref (parse-pi-args '("--no-codemap-inject")) 'no-codemap-inject)
  =>
  #t
) ;check
(check (pi-args-ref (parse-pi-args '("--codemap-file" "x.json")) 'codemap-file)
  =>
  "x.json"
) ;check
(check (pi-args-ref (parse-pi-args '("--codemap-file=x.json")) 'codemap-file)
  =>
  "x.json"
) ;check

;; 互斥：--codemap 与 --tour。
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--codemap" "--tour")))
) ;check-catch

;; 与既有工作流开关互斥。
(for-each
  (lambda (args)
    (check-catch 'pi-arg-error
      (pi-validate-review-flags (parse-pi-args args))
    ) ;check-catch
  ) ;lambda
  '(("--codemap" "--code-review")
    ("--codemap" "--simplify")
    ("--tour" "--red-green" "x")
    ("--codemap" "--blueprint" "x")
    ("--tour" "--plan-status"))
) ;for-each

;; --refresh / --codemap-file 只能配合 --codemap。
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--refresh")))
) ;check-catch
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--tour" "--refresh")))
) ;check-catch
(check-catch 'pi-arg-error
  (pi-validate-review-flags (parse-pi-args '("--codemap-file" "x.json")))
) ;check-catch

;; 合法组合。
(check (pi-validate-review-flags (parse-pi-args '("--codemap"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--codemap" "--refresh")))
  =>
  #t
) ;check
(check (pi-validate-review-flags (parse-pi-args '("--codemap" "--codemap-file" "x.json")))
  =>
  #t
) ;check
(check (pi-validate-review-flags (parse-pi-args '("--tour"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--tour" "src"))) => #t)
(check (pi-validate-review-flags (parse-pi-args '("--codemap" "--no-codemap-inject")))
  =>
  #t
) ;check

;; 未使用 compass 开关时默认值全为 #f。
(check (pi-args-ref (parse-pi-args '("hello")) 'codemap) => #f)
(check (pi-args-ref (parse-pi-args '("hello")) 'tour) => #f)
(check (pi-args-ref (parse-pi-args '("hello")) 'refresh) => #f)
(check (pi-args-ref (parse-pi-args '("hello")) 'codemap-file) => #f)
(check (pi-args-ref (parse-pi-args '("hello")) 'no-codemap-inject) => #f)

;; 主题取值辅助。
(check (pi-compass-tour-topic-arg (parse-pi-args '("--tour"))) => #f)
(check (pi-compass-tour-topic-arg (parse-pi-args '("--tour" "src"))) => "src")
(check (pi-compass-tour-topic-arg (parse-pi-args '("--codemap"))) => #f)

(check-report)
