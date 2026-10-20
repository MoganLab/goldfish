(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  (scheme process-context)
  (liii check)
  (liii string)
  (liii list)
  (liii path)
  (liii os)
  (liii json)
  (liii subprocess)
  (liii pi bash)
  (liii pi args)
  (liii pi compass args)
  (liii pi compass project)
  (liii pi compass generate)
  (liii pi compass store)
  (liii pi compass render)
  (liii pi compass tour)
  (liii pi compass prompt)
  (liii pi compass tools)
  (liii pi tools types)
) ;import

(check-set-mode! 'report-failed)

;; [048] 验收脚本：compass 代码库导航
;; 逐条对应 tools/pi/devel/048.md 验收点：参数解析与互斥、codemap 离线生成、
;; 缓存新鲜度（哈希变化重生成 + --refresh）、code tour、Scheme 适配、
;; 系统提示词注入、内置工具、输出分流、门禁与 23 个新增测试。
;; 外部命令一律 timeout 包裹；辅助函数见 048-support.scm。

(load "tools/pi/devel/048-support.scm")
(load "tools/pi/devel/048-support-cli.scm")

(%cleanup!)
(%check-arg-parsing!)
(%check-codemap-cli!)
(%check-mutual-exclusion!)
(%check-tour-cli!)
(%check-scheme-fixture!)
(%check-injection!)
(%check-tools!)

(dynamic-wind (lambda () #f)
  (lambda ()
    (%check-gates!)
    (%check-tests!)
    (%check-line-limits!)
    (display "=== 048 验收完成 ===\n")
  ) ;lambda
  (lambda () (%cleanup!) (check-report))
) ;dynamic-wind
