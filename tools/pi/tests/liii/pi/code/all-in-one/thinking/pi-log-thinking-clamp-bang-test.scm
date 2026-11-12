;; pi-log-thinking-clamp!
;; 显式 Thinking Level 被真实节点能力收敛时写一条可诊断的按天日志与 stderr 提示。
;;
;; 语法
;; ----
;; (pi-log-thinking-clamp! config chain-id provider model-id requested effective)
;;
;; 参数
;; ----
;; config : alist
;; 主会话配置，提供 config-dir。
;; chain-id、provider、model-id : string
;; 用于日志定位的链与真实物理模型。
;; requested、effective : string
;; 用户请求等级与收敛后的实际生效等级。
;;
;; 返回值
;; ----
;; unspecified
;; requested 与 effective 相同时不写日志、不输出 stderr。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii list)
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

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path TMP-DIR) :parents #t :exist-ok #t)

(define CONFIG (list (cons 'config-dir TMP-DIR)))

;; 1. 请求值被收敛：日志文件出现 [THINKING_CLAMP] 与 requested/effective
(pi-log-thinking-clamp! CONFIG "coding-fast" "google" "gemini-3.8-flash" "max" "high")
(let* ((log-file (resolve-alarm-log-path TMP-DIR))
       (content (path-read-text log-file))
      ) ;
  (check-true (string-contains? content "[THINKING_CLAMP]"))
  (check-true (string-contains? content "coding-fast"))
  (check-true (string-contains? content "requested=max"))
  (check-true (string-contains? content "effective=high"))
) ;let*

;; 2. 请求值未被改变时不写日志
(pi-log-thinking-clamp! CONFIG "coding-fast" "google" "gemini-3.8-flash" "high" "high")
(let* ((log-file (resolve-alarm-log-path TMP-DIR))
       (content (path-read-text log-file))
      ) ;
  (check (length (filter (lambda (line) (string-contains? line "requested=high"))
                    (string-split content "\n")
                  ) ;filter
    ) ;length
    =>
    0
  ) ;check
) ;let*

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
