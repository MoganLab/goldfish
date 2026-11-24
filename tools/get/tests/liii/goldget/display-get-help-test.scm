;; display-get-help
;; 输出 gf get 用法帮助。
;;
;; 语法
;; ----
;; (display-get-help)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; unspecified
;; 帮助文本写入当前输出端口。

(import (liii check) (liii string) (liii goldget))

(check-set-mode! 'report-failed)

(let* ((port (open-output-string))
       (orig (current-output-port))
       (_ (set! (current-output-port) port))
       (_ (display-get-help))
       (_ (set! (current-output-port) orig))
       (text (get-output-string port))
      ) ;
  (check-true (string-contains? text "Usage: gf get"))
  (check-true (string-contains? text "--from"))
  (check-true (string-contains? text "--ref"))
  (check-true (string-contains? text "name@org"))
) ;let

(check-report)
