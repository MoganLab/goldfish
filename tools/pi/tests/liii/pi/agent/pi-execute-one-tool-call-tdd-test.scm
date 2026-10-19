(import (scheme base)
        (scheme file)
        (liii check)
        (liii string)
        (liii pi)
        (liii pi agent tools)
        (liii pi red-green state)
) ;import

(check-set-mode! 'report-failed)

(define %impl-path "tools/pi/devel/tmp-046-impl.py")
(define %test-path "tools/pi/devel/test_tmp_046_gen.py")

(define (cleanup!)
  (for-each
    (lambda (p) (when (file-exists? p) (delete-file p)))
    (list %impl-path %test-path)
  ) ;for-each
) ;define

(define (tool-call name arguments)
  (list (cons "type" "toolCall")
    (cons "id" "call-1")
    (cons "name" name)
    (cons "arguments" arguments)
  ) ;list
) ;define

(define (message-text msg)
  (pi-extract-tool-result-text msg)
) ;define

(cleanup!)

;; RED 阶段：write 实现文件被工具层拒绝，不落盘，agent 循环不崩溃。
(pi-tdd-set-current-state! (pi-tdd-initial-state "实现登录"))
(let ((msg (pi-execute-one-tool-call
             (tool-call "write"
               (list (cons "path" %impl-path) (cons "content" "x = 1\n"))
             ) ;tool-call
             #f
             30
           ) ;pi-execute-one-tool-call
          ) ;
     ) ;
  (check (pi-tool-result-message-is-error msg) => #t)
  (check-true (string-contains? (message-text msg) "TDD RED phase violation"))
  (check (file-exists? %impl-path) => #f)
) ;let

;; RED 阶段：测试文件写入放行，并被记录。
(let ((msg (pi-execute-one-tool-call
             (tool-call "write"
               (list (cons "path" %test-path) (cons "content" "def test_x():\n    pass\n"))
             ) ;tool-call
             #f
             30
           ) ;pi-execute-one-tool-call
          ) ;
     ) ;
  (check (pi-tool-result-message-is-error msg) => #f)
  (check (file-exists? %test-path) => #t)
  (check (pi-tdd-state-test-files (pi-tdd-current-state)) => (list %test-path))
) ;let

;; GREEN 阶段：实现文件写入放行，并被记录。
(pi-tdd-set-current-state!
  (let-values (((next warning) (pi-tdd-advance-phase (pi-tdd-initial-state "实现登录") 'manual-advance))) ;pi-tdd-advance-phase
    
    next
  ) ;let-values
) ;pi-tdd-set-current-state!
(let ((msg (pi-execute-one-tool-call
             (tool-call "write"
               (list (cons "path" %impl-path) (cons "content" "def add(a, b):\n    return a + b\n"))
             ) ;tool-call
             #f
             30
           ) ;pi-execute-one-tool-call
          ) ;
     ) ;
  (check (pi-tool-result-message-is-error msg) => #f)
  (check (file-exists? %impl-path) => #t)
  (check (pi-tdd-state-impl-files (pi-tdd-current-state)) => (list %impl-path))
) ;let

;; bash 工具输出被检测：RED + 测试失败 → 自动推进到 GREEN。
(pi-tdd-set-current-state! (pi-tdd-initial-state "实现登录"))
(pi-execute-one-tool-call
  (tool-call "bash" (list (cons "command" "printf 'pytest: 1 failed, 2 passed\\n'")))
  #f
  30
) ;pi-execute-one-tool-call
(check (pi-tdd-state-phase (pi-tdd-current-state)) => 'green)
(let ((run (pi-tdd-state-last-test-run (pi-tdd-current-state))))
  (check (cdr (assoc 'failed run)) => 1)
  (check (cdr (assoc 'passed run)) => 2)
) ;let

;; 非测试 bash 命令不推进阶段。
(pi-tdd-set-current-state! (pi-tdd-initial-state "实现登录"))
(pi-execute-one-tool-call
  (tool-call "bash" (list (cons "command" "printf '3 passed\\n'")))
  #f
  30
) ;pi-execute-one-tool-call
(check (pi-tdd-state-phase (pi-tdd-current-state)) => 'red)
(check (pi-tdd-state-last-test-run (pi-tdd-current-state)) => #f)

(pi-tdd-set-current-state! #f)
(cleanup!)

(check-report)
