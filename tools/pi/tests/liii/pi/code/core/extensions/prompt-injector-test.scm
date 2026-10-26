(import (scheme base)
  (liii check)
  (liii pi ai message)
  (liii pi code core extensions)
) ;import

(check-set-mode! 'report-failed)

;; 1. 注册静态字符串 injector
(let* ((reg (make-extension-registry)))
  (register-prompt-injector! reg "INJECTOR_STATIC_1")
  (register-prompt-injector! reg "INJECTOR_STATIC_2")
  (let ((results (extension-registry-collect-prompt-injectors reg '())))
    (check results => '("INJECTOR_STATIC_1" "INJECTOR_STATIC_2"))))

;; 2. 注册过程 injector (无参 / 单参 tool-calls)
(let* ((reg (make-extension-registry)))
  ;; 过程 1：无参过程
  (register-prompt-injector! reg (lambda () "INJECTOR_THUNK"))
  ;; 过程 2：接收 tool-calls 动态生成
  (register-prompt-injector! reg
    (lambda (tool-calls)
      (if (null? tool-calls)
        "NO_TOOLS_YET"
        (string-append "TOOLS_SEEN:" (number->string (length tool-calls))))))
  ;; 过程 3：返回 #f 或空字符串时被安全忽略
  (register-prompt-injector! reg (lambda (tool-calls) #f))
  (register-prompt-injector! reg (lambda (tool-calls) ""))

  ;; 轮次 1：无工具调用
  (let ((res1 (extension-registry-collect-prompt-injectors reg '())))
    (check res1 => '("INJECTOR_THUNK" "NO_TOOLS_YET")))

  ;; 轮次 2：存在工具调用
  (let ((res2 (extension-registry-collect-prompt-injectors reg '(((name . "bash")) ((name . "read"))))))
    (check res2 => '("INJECTOR_THUNK" "TOOLS_SEEN:2")))

  ;; 轮次 3：真实链路下的 <pi-tool-call-block> record 列表
  (let* ((tc1 (pi-tool-call-block "tc-1" "write" '(("path" . "a.scm")) :thought-signature "sig123"))
         (tc2 (pi-tool-call-block "tc-2" "read" '(("path" . "b.scm")) :thought-signature "sig456"))
         (res3 (extension-registry-collect-prompt-injectors reg (list tc1 tc2))))
    (check res3 => '("INJECTOR_THUNK" "TOOLS_SEEN:2"))))

;; 3. 通过 ExtensionAPI 注册 prompt-injector
(let* ((api (make-extension-api))
       (reg (extension-api-registry api)))
  (register-prompt-injector! api "API_INJECTOR")
  (check (extension-registry-collect-prompt-injectors reg '()) => '("API_INJECTOR")))

(check-report)
