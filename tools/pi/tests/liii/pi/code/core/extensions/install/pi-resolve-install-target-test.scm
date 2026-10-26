(import (scheme base)
  (scheme process-context)
  (liii check)
  (liii path)
  (liii pi code core extensions install)
) ;import

;; 1. 测试全局目标路径（默认）
(let ((target (pi-resolve-install-target "demo-ext"
                :local? #f
                :agent-dir "/custom/agent"
                :cwd "/project/dir"
              ) ;pi-resolve-install-target
      ) ;target
     ) ;
  (check-true (string? target))
  (check-true (string=? target (path->string (path-resolve (path "/custom/agent/extensions/demo-ext")))))
) ;let

;; 2. 测试 --local 目标路径
(let ((target (pi-resolve-install-target "demo-ext"
                :local? #t
                :agent-dir "/custom/agent"
                :cwd "/project/dir"
              ) ;pi-resolve-install-target
      ) ;target
     ) ;
  (check-true (string? target))
  (check-true (string=? target (path->string (path-resolve (path "/project/dir/.pi/extensions/demo-ext")))))
) ;let

;; 3. 非法扩展名直接报错
(check-catch 'value-error (pi-resolve-install-target "../bad-ext" :local? #f))
(check-catch 'type-error (pi-resolve-install-target 123 :local? #f))

(check-report)
