;; %pi-app-record-bash!
;; 记录 Bash 命令执行结果至会话上下文。
;;
;; 语法
;; ----
;; (%pi-app-record-bash! app command text code excluded?)
;;
;; 参数
;; ----
;; app : pi-app?
;; 交互式应用实例。
;; command : string?
;; 执行的 Bash 命令。
;; text : string?
;; 命令输出文本。
;; code : integer?
;; 进程退出码。
;; excluded? : boolean?
;; 是否从会话历史中排除。
;;
;; 返回值
;; ----
;; unspecified

(import (scheme base)
  (liii check)
  (liii tui)
  (liii pi code modes interactive app)
  (liii pi code modes interactive app-bash)
) ;import

(check-set-mode! 'report-failed)

;; 1. excluded? 为 #t 时不记录
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (recorded #f)
       (app (make-pi-app
              :terminal    vterm
              :record-sink (lambda (cmd txt c) (set! recorded #t))
            ) ;make-pi-app
       ) ;app
      ) ;
  (%pi-app-record-bash! app "ls" "file.txt" 0 #t)
  (check-false recorded)
) ;let*

;; 2. excluded? 为 #f 时触发 record-sink
(let* ((vterm (make-virtual-terminal :width 80 :height 24))
       (recorded '())
       (app
         (make-pi-app
           :terminal    vterm
           :record-sink (lambda (cmd txt c) (set! recorded (list cmd txt c)))
         ) ;make-pi-app
       ) ;app
      ) ;
  (%pi-app-record-bash! app "pwd" "/home" 0 #f)
  (check recorded => (list "pwd" "/home" 0))
) ;let*

(check-report)
