(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi extension)
  (liii pi agent tools)
) ;import

(check-set-mode! 'report-failed)

;; 1. pi-emit! 正确识别 block + terminate
(let* ((ctx (make-extension-context)))
  (pi-on! ctx 'tool_call
    (lambda (payload)
      ;; 形式 1: '((block . #t) (reason . "Stop now") (terminate . #t))
      '((block . #t) (reason . "Stop now") (terminate . #t))))
  (let ((res (pi-emit! ctx 'tool_call '((tool . "test")))))
    (check (cdr (assq 'action res)) => 'block)
    (check (cdr (assq 'reason res)) => "Stop now")
    (check (cdr (assq 'terminate res)) => #t)))

;; 2. 形式 2: '(block (reason . "Terminating via block") (terminate . #t))
(let* ((ctx (make-extension-context)))
  (pi-on! ctx 'tool_call
    (lambda (payload)
      '(block (reason . "Terminating via block") (terminate . #t))))
  (let ((res (pi-emit! ctx 'tool_call '((tool . "test")))))
    (check (cdr (assq 'action res)) => 'block)
    (check (cdr (assq 'reason res)) => "Terminating via block")
    (check (cdr (assq 'terminate res)) => #t)))

;; 3. 批量执行中发生 terminate：截断批次后续未执行的工具，并生成 skipped 消息
(let* ((reg (make-extension-registry))
       (tool2-executed? #f))
  (register-tool! reg
    (make-extension-tool-def "t1" "T1" '() (lambda (args) "t1 ok")))
  (register-tool! reg
    (make-extension-tool-def "t2" "T2" '()
      (lambda (args)
        (set! tool2-executed? #t)
        "t2 ok")))

  (pi-on! reg 'tool_call
    (lambda (payload)
      (if (string=? (cdr (assq 'tool payload)) "t1")
        '((block . #t) (reason . "Blocked with terminate") (terminate . #t))
        'continue)))

  (with-extension-registry reg
    (lambda ()
      (let* ((calls '(((id . "c1") (name . "t1") (arguments . ()))
                      ((id . "c2") (name . "t2") (arguments . ()))))
            )
        (let-values (((res terminated?)
                      (pi-execute-tool-calls-with-status calls #f 10)))
          ;; 整个批次被标记为 terminated
          (check terminated? => #t)
          (check (length res) => 2)
          ;; 第一个工具被 block 并带有报错
          (let ((m1 (car res)))
            (check (pi-tool-result-message-is-error m1) => #t)
            (check (pi-extract-tool-result-text m1) => "Blocked with terminate"))
          ;; 第二个工具被跳过，没有实际执行，带有 skipped 提示
          (let ((m2 (cadr res)))
            (check (pi-tool-result-message-is-error m2) => #t)
            (check tool2-executed? => #f)
            (check (pi-extract-tool-result-text m2)
                   => "Tool call skipped: previous tool call terminated the run")))))))

(check-report)
