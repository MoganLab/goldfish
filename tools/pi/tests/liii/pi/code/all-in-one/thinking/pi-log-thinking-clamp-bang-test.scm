;; pi-log-thinking-clamp!
;; 显式 Thinking Level 被真实节点能力收敛时写按天日志并通知前台一次。
;;
;; 语法
;; ----
;; (pi-log-thinking-clamp! config chain-id provider model-id requested effective)
;;
;; 参数
;; ----
;; config : <pi-config>
;; 主会话配置，提供 config-dir 与可选的 emit 回调。
;; chain-id、provider、model-id : string
;; 用于日志定位的链与真实物理模型。
;; requested、effective : string
;; 用户请求等级与收敛后的实际生效等级。
;;
;; 返回值
;; ----
;; unspecified
;; requested 与 effective 相同时不写日志、不发事件、不输出 stderr。
;;
;; 说明
;; ----
;; 收敛诊断写按天日志的 [THINKING_CLAMP] 行，并在没有 emit 时退回 stderr，
;; 使脚本模式仍能看到收敛结果。日志断言使用隔离目录，避免污染用户日志。
;; 前台 thinking_clamp 事件通道不在本测试范围。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii pi ai config runtime)
  (liii pi code all-in-one alarm)
  (liii pi code all-in-one thinking)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "pi-thinking-clamp-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(define (with-fresh-dir thunk)
  (when (file-exists? TMP-DIR)
    (rmtree TMP-DIR #t)
  ) ;when
  (path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)
  (thunk)
) ;define

(define (log-content)
  (let ((file (resolve-alarm-log-path TMP-DIR)))
    (if (file-exists? (path->string file)) (path-read-text (path->string file)) "")
  ) ;let
) ;define

(define (capture-stderr thunk)
  (let ((prev (if (defined? '*stderr*) *stderr* (current-error-port)))
        (port (open-output-string))
       ) ;
    (set! *stderr* port)
    (dynamic-wind (lambda () #f) thunk (lambda () (set! *stderr* prev)))
    (get-output-string port)
  ) ;let
) ;define

;; 1. 无 emit 的真实 <pi-config>：记录 [THINKING_CLAMP] 日志并回退 stderr
(with-fresh-dir
  (lambda ()
    (let* ((cfg (make-pi-config :config-dir (path TMP-DIR)))
           (stderr (capture-stderr (lambda ()
                                     (pi-log-thinking-clamp! cfg "coding-fast"
                                       "google" "gemini-3.8-flash" "max" "high"
                                     ) ;pi-log-thinking-clamp!
                                   ) ;lambda
                   ) ;capture-stderr
           ) ;stderr
           (content (log-content))
          ) ;
      (check-true (string-contains? content "[THINKING_CLAMP]"))
      (check-true (string-contains? content "chain=coding-fast"))
      (check-true (string-contains? content "requested=max"))
      (check-true (string-contains? content "effective=high"))
      (check-true (string-contains? stderr "[all-in-one] thinking level clamped: max -> high")
      ) ;check-true
    ) ;let*
  ) ;lambda
) ;with-fresh-dir

;; 2. 未发生收敛：不写日志、不输出 stderr
(with-fresh-dir
  (lambda ()
    (let* ((events '())
           (cfg
             (make-pi-config
               :config-dir (path TMP-DIR)
               :extras
               (list
                 (cons 'emit (lambda (type payload) (set! events (cons type events))))
               ) ;list
             ) ;make-pi-config
           ) ;cfg
           (stderr (capture-stderr (lambda ()
                                     (pi-log-thinking-clamp! cfg "coding-fast"
                                       "google" "gemini-3.8-flash" "high" "high"
                                     ) ;pi-log-thinking-clamp!
                                   ) ;lambda
                   ) ;capture-stderr
           ) ;stderr
          ) ;
      (check events => '())
      (check (log-content) => "")
      (check stderr => "")
    ) ;let*
  ) ;lambda
) ;with-fresh-dir

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
