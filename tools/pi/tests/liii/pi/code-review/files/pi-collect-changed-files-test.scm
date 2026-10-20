(import (liii check)
        (liii string)
        (liii pi code-review files)
) ;import

(check-set-mode! 'report-failed)

;; pi-collect-changed-files
;; 用注入的 runner 模拟 git 命令，避免单测依赖真实仓库状态。

(define (make-runner table calls-box)
  (lambda (cmd timeout)
    (vector-set! calls-box 0 (cons cmd (vector-ref calls-box 0)))
    (let ((entry (assoc cmd table)))
      (if entry
        (values (cadr entry) (caddr entry))
        (values "" #t)
      ) ;if
    ) ;let
  ) ;lambda
) ;define

(define (make-calls-box)
  (vector '())
) ;define

(define (calls box)
  (reverse (vector-ref box 0))
) ;define

;; 1. 主命令成功且有变更
(let* ((box (make-calls-box))
       (runner (make-runner
                 (list (list "git diff --name-status HEAD" "M\tsrc/app.ts\nA\tsrc/new.py\n" #f))
                 box
               ) ;make-runner
       ) ;runner
       (res (pi-collect-changed-files :runner runner))
      ) ;
  (check (cdr (assoc 'source res)) => "diff")
  (check (cdr (assoc 'paths res)) => '("src/app.ts" "src/new.py"))
  (check (cdr (assoc 'total res)) => 2)
  (check (cdr (assoc 'truncated? res)) => #f)
  (check (cdr (assoc 'message res)) => #f)
  (check (calls box) => '("git diff --name-status HEAD"))
) ;let*

;; 2. 主命令失败时回退 HEAD~1
(let* ((box (make-calls-box))
       (runner (make-runner
                 (list (list "git diff --name-status HEAD" "" #t)
                       (list "git diff --name-status HEAD~1" "M\tfallback.ts\n" #f)
                 ) ;list
                 box
               ) ;make-runner
       ) ;runner
       (res (pi-collect-changed-files :runner runner))
      ) ;
  (check (cdr (assoc 'source res)) => "fallback")
  (check (cdr (assoc 'paths res)) => '("fallback.ts"))
  (check (length (calls box)) => 2)
) ;let*

;; 3. 主命令成功但无变更，也回退
(let* ((box (make-calls-box))
       (runner (make-runner
                 (list (list "git diff --name-status HEAD" "" #f)
                       (list "git diff --name-status HEAD~1" "A\told.ts\n" #f)
                 ) ;list
                 box
               ) ;make-runner
       ) ;runner
       (res (pi-collect-changed-files :runner runner))
      ) ;
  (check (cdr (assoc 'source res)) => "fallback")
  (check (cdr (assoc 'paths res)) => '("old.ts"))
) ;let*

;; 4. 两者都失败/为空
(let* ((box (make-calls-box))
       (runner (make-runner '() box))
       (res (pi-collect-changed-files :runner runner))
      ) ;
  (check (cdr (assoc 'paths res)) => '())
  (check (cdr (assoc 'source res)) => #f)
  (check (length (calls box)) => 2)
) ;let*

;; 5. --staged 使用 --cached 命令
(let* ((box (make-calls-box))
       (runner (make-runner
                 (list (list "git diff --name-status --cached" "M\tstaged.ts\n" #f))
                 box
               ) ;make-runner
       ) ;runner
       (res (pi-collect-changed-files :runner runner :staged #t))
      ) ;
  (check (cdr (assoc 'paths res)) => '("staged.ts"))
  (check (calls box) => '("git diff --name-status --cached"))
) ;let*

;; 6. 超过上限截断并给出提示
(let* ((many (string-append
               "M\tf01.ts\nM\tf02.ts\nM\tf03.ts\nM\tf04.ts\nM\tf05.ts\n"
               "M\tf06.ts\nM\tf07.ts\nM\tf08.ts\nM\tf09.ts\nM\tf10.ts\n"
               "M\tf11.ts\nM\tf12.ts\n"
             ) ;string-append
       ) ;many
       (box (make-calls-box))
       (runner (make-runner
                 (list (list "git diff --name-status HEAD" many #f))
                 box
               ) ;make-runner
       ) ;runner
       (res (pi-collect-changed-files :runner runner))
      ) ;
  (check (length (cdr (assoc 'paths res))) => 10)
  (check (cdr (assoc 'total res)) => 12)
  (check (cdr (assoc 'truncated? res)) => #t)
  (check (cdr (assoc 'message res)) => "Reviewing first 10 of 12 changed files.")
) ;let*

;; 7. 显式文件列表：不调用 git
(let* ((box (make-calls-box))
       (runner (make-runner '() box))
       (res (pi-collect-changed-files :runner runner :files '("src/a.ts" "src/b.py")))
      ) ;
  (check (cdr (assoc 'source res)) => "explicit")
  (check (cdr (assoc 'paths res)) => '("src/a.ts" "src/b.py"))
  (check (calls box) => '())
) ;let*

;; 8. 自定义 ref
(let* ((box (make-calls-box))
       (runner (make-runner
                 (list (list "git diff --name-status main" "M\tfrom-main.ts\n" #f))
                 box
               ) ;make-runner
       ) ;runner
       (res (pi-collect-changed-files :runner runner :ref "main"))
      ) ;
  (check (cdr (assoc 'paths res)) => '("from-main.ts"))
) ;let*

(check-report)
