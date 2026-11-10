;; wt-detect-alt-enter-status
;; 识别 JSONC 配置中 Alt+Enter 的默认冲突、已解绑或用户自定义状态。
;;
;; 语法
;; ----
;; (wt-detect-alt-enter-status text) -> symbol
;;
;; 参数
;; ----
;; text : string
;; Windows Terminal settings.json 的 JSONC 文本。
;;
;; 返回值
;; ----
;; symbol
;; 返回 default-conflict、already-unbound 或 user-custom。
;;

(import (scheme base)
  (liii check)
  (liii path)
  (liii pi code cli windows-terminal)
) ;import

(check-set-mode! 'report-failed)

(define %fixtures-dir
  (cond ((path-exists? (path "tests/fixtures/windows-terminal"))
         "tests/fixtures/windows-terminal"
        ) ;
        ((path-exists? (path "tools/pi/tests/fixtures/windows-terminal"))
         "tools/pi/tests/fixtures/windows-terminal"
        ) ;
        (else "../../tools/pi/tests/fixtures/windows-terminal")
  ) ;cond
) ;define

(define (read-fixture name)
  (path-read-text (path-join %fixtures-dir name))
) ;define

;; 1. 默认无绑定 -> default-conflict
(check (wt-detect-alt-enter-status (read-fixture "default-binding.jsonc"))
  =>
  'default-conflict
) ;check

;; 2. 显式绑定 toggleFullscreen -> default-conflict
(check (wt-detect-alt-enter-status (read-fixture "explicit-toggle-fullscreen.jsonc"))
  =>
  'default-conflict
) ;check

;; 3. 已是 unbound -> already-unbound
(check (wt-detect-alt-enter-status (read-fixture "already-unbound.jsonc"))
  =>
  'already-unbound
) ;check

;; 4. 用户自定义绑定为 copy -> user-custom
(check (wt-detect-alt-enter-status (read-fixture "custom-binding.jsonc"))
  =>
  'user-custom
) ;check

;; 5. actions 数组为空 -> default-conflict
(check (wt-detect-alt-enter-status (read-fixture "actions-empty.jsonc"))
  =>
  'default-conflict
) ;check

;; 6. 无 actions 字段 -> default-conflict
(check (wt-detect-alt-enter-status (read-fixture "no-actions.jsonc"))
  =>
  'default-conflict
) ;check

;; 7. keybindings 旧格式无 alt+enter -> default-conflict
(check (wt-detect-alt-enter-status (read-fixture "keybindings-old.jsonc"))
  =>
  'default-conflict
) ;check

;; 8. 复杂 JSONC（注释、尾逗号、字符串包含大括号与注释符号）
(check (wt-detect-alt-enter-status (read-fixture "jsonc-comments-trailing-comma.jsonc")
       ) ;wt-detect-alt-enter-status
  =>
  'default-conflict
) ;check

;; 9. 空字符串或格式异常回退 default-conflict
(check (wt-detect-alt-enter-status "") => 'default-conflict)
(check (wt-detect-alt-enter-status "{ bad json") => 'default-conflict)

(check-report)
