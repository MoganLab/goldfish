(import (liii check)
        (liii pi args)
        (liii pi modes blueprint)
) ;import

(check-set-mode! 'report-failed)

(define (parsed . args)
  (parse-pi-args args)
) ;define

;; pi-blueprint-extend-tool-names
;; 自动注入三个 blueprint 工具，并尊重 --exclude-tools / --no-tools。
(check (pi-blueprint-extend-tool-names '("read" "bash" "edit" "write")
         (parsed "--blueprint" "目标")
       ) ;pi-blueprint-extend-tool-names
  =>
  '("read" "bash" "edit" "write"
    "blueprint_update"
    "blueprint_status"
    "blueprint_next"
   ) ;list
) ;check

;; 已在列表中的工具不重复添加。
(check (pi-blueprint-extend-tool-names '("read" "blueprint_status")
         (parsed "--plan-next")
       ) ;pi-blueprint-extend-tool-names
  =>
  '("read" "blueprint_status" "blueprint_update" "blueprint_next")
) ;check

;; --exclude-tools 关闭指定工具。
(check (pi-blueprint-extend-tool-names '("read")
         (parsed "--plan-next" "--exclude-tools" "blueprint_next")
       ) ;pi-blueprint-extend-tool-names
  =>
  '("read" "blueprint_update" "blueprint_status")
) ;check

(check (pi-blueprint-extend-tool-names '("read")
         (parsed "--plan-next" "--exclude-tools"
           "blueprint_update,blueprint_status,blueprint_next"
         ) ;parsed
       ) ;pi-blueprint-extend-tool-names
  =>
  '("read")
) ;check

;; --no-tools 时不注入。
(check (pi-blueprint-extend-tool-names '()
         (parsed "--blueprint" "目标" "--no-tools")
       ) ;pi-blueprint-extend-tool-names
  =>
  '()
) ;check

(check-report)
