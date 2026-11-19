;; %poll-mouse-reset!
;; 清空 click 合成所用的 press 状态。
;;
;; 语法
;; ----
;; (%poll-mouse-reset!)
;;
;; 参数
;; ----
;; 无。
;;
;; 返回值
;; ----
;; unspecified
;; 未指定具体返回值。

(import (scheme base) (liii check) (liii tui mouse) (liii tui terminal poll))

(check-set-mode! 'report-failed)

;; 重置后 press 状态被清空，后续 release 不再合成 click
(let ((press (%poll-mouse-feed '(27 91 60 48 59 49 48 59 53 77))))
  (check (mouse-event-type press) => 'press)
  (%poll-mouse-reset!)
  (let ((release (%poll-mouse-feed '(27 91 60 48 59 49 48 59 53 109))))
    (check (mouse-event-type release) => 'release)
  ) ;let
) ;let

(check-report)
