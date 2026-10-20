(import (liii check)
        (liii pi args)
        (liii pi modes red-green)
) ;import

(check-set-mode! 'report-failed)

;; pi-tdd-extend-tool-names
;; --red-green 模式自动注入三个逃生舱工具，并尊重 --exclude-tools / --no-tools。

(define (parsed . args)
  (parse-pi-args args)
) ;define

(check (pi-tdd-extend-tool-names '("read" "bash" "edit" "write")
         (parsed "--red-green" "任务")
       ) ;pi-tdd-extend-tool-names
  =>
  '("read" "bash" "edit" "write" "tdd_status" "tdd_advance" "tdd_reset")
) ;check

;; 已在列表中的工具不重复添加。
(check (pi-tdd-extend-tool-names '("read" "tdd_status")
         (parsed "--red-green" "任务")
       ) ;pi-tdd-extend-tool-names
  =>
  '("read" "tdd_status" "tdd_advance" "tdd_reset")
) ;check

;; --exclude-tools 关闭指定 TDD 工具。
(check (pi-tdd-extend-tool-names '("read")
         (parsed "--red-green" "任务" "--exclude-tools" "tdd_advance")
       ) ;pi-tdd-extend-tool-names
  =>
  '("read" "tdd_status" "tdd_reset")
) ;check

(check (pi-tdd-extend-tool-names '("read")
         (parsed "--red-green" "任务" "--exclude-tools"
           "tdd_status,tdd_advance,tdd_reset"
         ) ;parsed
       ) ;pi-tdd-extend-tool-names
  =>
  '("read")
) ;check

;; --no-tools 时不注入。
(check (pi-tdd-extend-tool-names '()
         (parsed "--red-green" "任务" "--no-tools")
       ) ;pi-tdd-extend-tool-names
  =>
  '()
) ;check

(check-report)
