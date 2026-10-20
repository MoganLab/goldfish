(import (liii check)
        (liii string)
        (liii path)
        (liii pi args)
        (liii pi modes blueprint common)
) ;import

(check-set-mode! 'report-failed)

;; pi-bp-mode-state-path
;; 默认落到工程内 .pi/blueprint.json。
(check (pi-bp-mode-state-path (parse-pi-args '("--plan-status")))
  =>
  (path->string (path-join (path (path->string (path-cwd)))
                  ".pi"
                  "blueprint.json"
                ) ;path-join
  ) ;path->string
) ;check

;; --blueprint-file 支持相对路径（相对当前工作目录）。
(check (pi-bp-mode-state-path
         (parse-pi-args '("--plan-status" "--blueprint-file" "tmp/plan.json"))
       ) ;pi-bp-mode-state-path
  =>
  (path->string (path-join (path (path->string (path-cwd)))
                  "tmp/plan.json"
                ) ;path-join
  ) ;path->string
) ;check

;; 绝对路径原样使用。
(let ((abs-path (path->string (path-resolve (path-join (path-temp-dir) "gf-plan.json")))))
  (check (pi-bp-mode-state-path
           (parse-pi-args (list "--plan-status" "--blueprint-file" abs-path))
         ) ;pi-bp-mode-state-path
    =>
    abs-path
  ) ;check
) ;let

;; 空字符串回退到默认路径。
(check (pi-bp-mode-state-path
         (parse-pi-args '("--plan-status" "--blueprint-file="))
       ) ;pi-bp-mode-state-path
  =>
  (path->string (path-join (path (path->string (path-cwd)))
                  ".pi"
                  "blueprint.json"
                ) ;path-join
  ) ;path->string
) ;check

(check-report)
