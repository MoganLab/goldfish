;; display-remove-help
;; 输出 gf remove 用法帮助。
;;
;; 语法
;; ----
;; (display-remove-help)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; unspecified
;; 帮助文本写入当前输出端口。

(import (liii check) (liii string) (liii goldremove))

(check-set-mode! 'report-failed)

(let* ((port (open-output-string))
       (orig (current-output-port))
       (_ (set! (current-output-port) port))
       (_ (display-remove-help))
       (_ (set! (current-output-port) orig))
       (text (get-output-string port))
      ) ;
  (check-true (string-contains? text "Usage: gf remove"))
  (check-true (string-contains? text "-g"))
  (check-true (string-contains? text "name@org"))
  (check-true (string-contains? text "working copy"))
) ;let

(check-report)
