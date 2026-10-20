(import (liii check)
        (liii list)
        (liii pi compass tools)
) ;import

(check-set-mode! 'report-failed)

;; pi-compass-extend-tool-names
;; 默认追加两个 compass 工具。
(check (pi-compass-extend-tool-names '("read" "bash" "edit" "write") '())
  =>
  '("read" "bash" "edit" "write" "codebase_map" "code_tour")
) ;check

;; 已存在时不重复追加。
(check (pi-compass-extend-tool-names '("read" "code_tour") '())
  =>
  '("read" "code_tour" "codebase_map")
) ;check

;; --no-tools 关闭。
(check (pi-compass-extend-tool-names '("read") '((no-tools . #t)))
  =>
  '("read")
) ;check

;; --exclude-tools 精确排除。
(check (pi-compass-extend-tool-names '("read") '((exclude-tools . "code_tour")))
  =>
  '("read" "codebase_map")
) ;check

;; 显式 --tools 白名单不自动追加（偏差 #34）。
(check (pi-compass-extend-tool-names '("read") '((tools . "read")))
  =>
  '("read")
) ;check

(check (pi-compass-extend-tool-names '()
         '((tools . "read,codebase_map"))
       ) ;pi-compass-extend-tool-names
  =>
  '()
) ;check

(check-report)
