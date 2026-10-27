(import (liii check) (liii pi code core extensions))

(check-set-mode! 'report-failed)

;; extension-registry-collect-prompt-injectors
;; 执行注册表中的提示注入器，并收集非空字符串结果。
;;
;; 语法
;; ----
;; (extension-registry-collect-prompt-injectors target [tool-calls]) -> list
;;
;; 参数
;; ----
;; target : extension-registry | extension-api | extension-context
;; 扩展注册表或能够定位注册表的对象。
;; tool-calls : list
;; 可选的工具调用列表，会传给接收参数的注入器。
;;
;; 返回值
;; ----
;; list
;; 按注册顺序排列的非空提示字符串；字符串注入器直接返回其内容。
;; 注入器抛出的错误会被隔离，不会中断其他注入器。
;;
;; 示例
;; ----
;; (extension-registry-collect-prompt-injectors registry '())
;; => '("Project rules" "Use read first")

(let ((registry (make-extension-registry)))
  (register-prompt-injector! registry "Project rules")
  (register-prompt-injector! registry "")
  (register-prompt-injector! registry
    (lambda (tool-calls) (if (null? tool-calls) "Use read first" "Tool active"))
  ) ;register-prompt-injector!
  (check (extension-registry-collect-prompt-injectors registry '())
    =>
    '("Project rules" "Use read first")
  ) ;check
  (check (extension-registry-collect-prompt-injectors registry '(("bash")))
    =>
    '("Project rules" "Tool active")
  ) ;check
) ;let

(check-report)
