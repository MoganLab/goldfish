;; pi-pending-hint-line
;; 排队消息撤回提示行文案（对齐上游 pi interactive-mode dequeueHint）。
;;
;; 语法
;; ----
;; (pi-pending-hint-line [macos?])
;;
;; 参数
;; ----
;; macos? : boolean? (可选)
;; 是否为 macOS 平台；缺省时按 (os-macos?) 判断。
;;
;; 返回值
;; ------
;; string?
;; 返回提示行文本，例如 "↳ option+up to edit all queued messages"。

(import (scheme base)
  (liii check)
  (liii os)
  (liii pi code modes interactive views)
) ;import

(check-set-mode! 'report-failed)

;; 指定 macos? 为 #t 时输出 option+up
(check (pi-pending-hint-line #t) => "↳ option+up to edit all queued messages")

;; 指定 macos? 为 #f 时输出 alt+up（Linux 与 Windows）
(check (pi-pending-hint-line #f) => "↳ alt+up to edit all queued messages")

;; 缺省时按当前平台输出
(check (pi-pending-hint-line)
  =>
  (if (os-macos?)
    "↳ option+up to edit all queued messages"
    "↳ alt+up to edit all queued messages"
  ) ;if
) ;check

(check-report)
