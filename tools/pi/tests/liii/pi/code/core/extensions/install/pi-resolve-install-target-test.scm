;; pi-resolve-install-target
;; 解析扩展安装的目标目录路径。
;;
;; 语法
;; ----
;; (pi-resolve-install-target name . opts)
;;
;; 参数
;; ----
;; name : string
;; 扩展名，必须符合命名规范。
;;
;; opts : list?
;; 可选关键字参数，支持 :local?、:agent-dir、:cwd 等。
;;
;; 返回值
;; ----
;; string
;; 目标目录绝对路径。

(import (scheme base)
        (liii check)
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
