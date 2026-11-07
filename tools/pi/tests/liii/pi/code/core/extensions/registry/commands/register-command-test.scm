;; register-command!
;; 注册扩展命令定义并验证命令查找行为。
;;
;; 语法
;; ----
;; (register-command! target command-def)
;;
;; 参数
;; ----
;; target : (or extension-registry extension-api extension-context)
;; 目标对象。
;;
;; command-def : (or extension-command-def alist)
;; 命令定义。
;;
;; 返回值
;; ----
;; extension-command-def
;; 规范化后注册的命令定义记录。

(import (liii check) (liii pi code core extensions))

(check-set-mode! 'report-failed)

;; 1. make-extension-command-def 构造与访问器
(let ((cmd (make-extension-command-def "tdd"
             "Run TDD workflow"
             (lambda (args ctx) "tdd-executed")
             :params '((mode . "strict"))
           ) ;make-extension-command-def
      ) ;cmd
     ) ;
  (check (extension-command-def? cmd) => #t)
  (check (extension-command-def-name cmd) => "tdd")
  (check (extension-command-def-description cmd) => "Run TDD workflow")
  (check (procedure? (extension-command-def-handler cmd)) => #t)
  (check (pair? (extension-command-def-params cmd)) => #t)
) ;let

;; 2. register-command! 注册与查询
(let* ((api (make-extension-api))
       (cmd (make-extension-command-def "review"
              "Run code review"
              (lambda (args ctx) "reviewed")
            ) ;make-extension-command-def
       ) ;cmd
      ) ;
  (register-command! api cmd)
  (let ((found (find-extension-command api "review")))
    (check (extension-command-def? found) => #t)
    (check (extension-command-def-name found) => "review")
    (check ((extension-command-def-handler found) '() #f) => "reviewed")
  ) ;let
) ;let*

;; 3. extension-register-command! 别名与 registry 支持
(let* ((reg (make-extension-registry))
       (cmd (make-extension-command-def "status" "Show status" (lambda (args ctx) "ok"))
       ) ;cmd
      ) ;
  (extension-register-command! reg cmd)
  (let ((found (find-extension-command reg "status")))
    (check (extension-command-def? found) => #t)
    (check (extension-command-def-name found) => "status")
  ) ;let
) ;let*

;; 4. 校验错误处理
(check-catch 'type-error
  (make-extension-command-def "" "desc" (lambda (a c) #t))
) ;check-catch
(check-catch 'type-error
  (make-extension-command-def "name" 123 (lambda (a c) #t))
) ;check-catch
(check-catch 'type-error
  (make-extension-command-def "name" "desc" "not-a-handler")
) ;check-catch
(check-catch 'type-error
  (register-command! "bad-target"
    (make-extension-command-def "n" "d" (lambda (a c) #t))
  ) ;register-command!
) ;check-catch

(check-report)
