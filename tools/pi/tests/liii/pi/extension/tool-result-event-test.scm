(import (scheme base)
  (liii check)
  (liii pi extension)
  (liii pi agent tools)
) ;import

(check-set-mode! 'report-failed)

;; 1. 工具执行完毕后正常派发 tool_result 事件
(let* ((reg (make-extension-registry))
       (observed-results '()))
  (register-tool! reg
    (make-extension-tool-def "math_tool"
      "Math tool"
      '()
      (lambda (args) "result:42")))

  (pi-on! reg 'tool_result
    (lambda (payload)
      (set! observed-results (cons payload observed-results))))

  (with-extension-registry reg
    (lambda ()
      (let* ((call '((id . "call_math")
                     (name . "math_tool")
                     (arguments . ((num . 42)))))
             (msg (pi-execute-one-tool-call call #f 10)))
        (check (length observed-results) => 1)
        (let ((ev (car observed-results)))
          (check (cdr (assq 'name ev)) => "math_tool")
          (check (cdr (assq 'id ev)) => "call_math")
          (check (cdr (assq 'input ev)) => '((num . 42)))
          (check (cdr (assq 'result ev)) => "result:42")
          (check (cdr (assq 'isError ev)) => #f))))))

(check-report)
