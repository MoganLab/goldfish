;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; pi AI 层异常请求日志
;;
;; 目的：凡是异常的 HTTP 请求（429/403/5xx、网络错误、流式空闲超时等），
;; 都以 ERROR 级别落盘，并把服务商返回的响应体原文（JSON / SSE）原样写入，
;; 便于事后定位限流、额度耗尽、供应商故障等问题。
;;
;; 日志目录固定为 ~/.gf/agent/logs（不存在时自动创建），
;; 日志文件按日期拆分：~/.gf/agent/logs/pi-YYYY-MM-DD.log
;;
;; 开关：PI_LOG=off|0|false|no 关闭；写入失败静默忽略，绝不影响请求本身。
;;
;; 目录解析为什么不复用 (liii pi code config)：code 层已经依赖 ai 层，
;; 反向 import 会形成循环依赖，因此这里直接展开 ~/.gf/agent/logs。

(define-library (liii pi ai log)
  (import (scheme base)
    (scheme char)
    (scheme process-context)
    (liii string)
    (liii os)
    (liii path)
    (liii date-time)
    (liii logging)
    (liii pi ai limits)
  ) ;import
  (export pi-log-dir pi-log-file-path pi-log-init! pi-log-reset! pi-log-enabled?
    pi-log-error! pi-log-http-error!
  ) ;export
  (begin

    ;; 初始化后固定下来的日志目录（path 对象），#f 表示使用默认目录 ~/.gf/agent/logs
    (define *log-dir* #f)
    ;; 目录是否已创建成功（失败一次后不再重复尝试）
    (define *log-dir-ready?* #f)
    (define *log-dir-failed?* #f)
    ;; 静默回调：不向 stderr 输出（(liii logging) 的默认回调会打印到 stderr）
    (define (%noop-handler msg)
      #f
    ) ;define

    (define (%non-empty-string? obj)
      (and (string? obj) (not (string-null? (string-trim-both obj))))
    ) ;define

    (define (%env name)
      (get-environment-variable name)
    ) ;define

    ;; 日志目录固定为 ~/.gf/agent/logs（显式初始化时可覆盖，仅测试使用）
    (define (pi-log-dir)
      (path-join (path-expanduser (path "~/.gf/agent")) "logs")
    ) ;define

    (define (%current-log-dir)
      (or *log-dir* (pi-log-dir))
    ) ;define

    ;; 日志文件：<日志目录>/pi-YYYY-MM-DD.log
    (define (pi-log-file-path)
      (path-join (%current-log-dir)
        (string-append "pi-"
          (date->iso-8601 (timestamp-date (current-system-timestamp)))
          ".log"
        ) ;string-append
      ) ;path-join
    ) ;define

    ;; 文件日志开关（默认开启）
    (define (pi-log-enabled?)
      (let ((val (%env "PI_LOG")))
        (if (not (%non-empty-string? val))
          #t
          (not (member (string-downcase (string-trim-both val)) '("off" "0"
                                                                  "false" "no"))
          ) ;not
        ) ;if
      ) ;let
    ) ;define

    (define (%log-level-from-env)
      (let ((val (%env "PI_LOG_LEVEL")))
        (if (not (%non-empty-string? val))
          INFO
          (case (string-downcase (string-trim-both val))
                ((emergency) EMERGENCY)
                ((alert) ALERT)
                ((critical) CRITICAL)
                ((error) ERROR)
                ((warning) WARNING)
                ((notice) NOTICE)
                ((debug) DEBUG)
                (else INFO)
          ) ;case
        ) ;if
      ) ;let
    ) ;define

    ;; 幂等初始化：确定日志目录并创建出来
    (define* (pi-log-init! (dir #f))
      (when dir
        (set! *log-dir* (path dir))
      ) ;when
      (unless *log-dir*
        (set! *log-dir* (pi-log-dir))
      ) ;unless
      (unless (or *log-dir-ready?* *log-dir-failed?*)
        (catch #t
          (lambda () (path-mkdir *log-dir* #t #t) (set! *log-dir-ready?* #t))
          (lambda args (set! *log-dir-failed?* #t))
        ) ;catch
      ) ;unless
      (and *log-dir-ready?* (pi-log-enabled?))
    ) ;define*

    ;; 关闭文件端口并把回调换成空处理器
    ;;
    ;; 之所以每次写完就关闭而不常开：s7 的 (exit N) 在退出钩子之后会先 s7_quit
    ;; 再读退出码（见 issue #1399），钩子里只要有分配就可能把退出码吞成 0。
    ;; 这里让退出时 *log-file-port* 为 #f，钩子变成空操作，退出码才能保住。
    (define (%detach-handler!)
      (catch #t (lambda () (log-set-callback! %noop-handler)) (lambda args #f))
    ) ;define

    ;; 关闭文件端口并清空状态（切换日志目录时使用）
    (define (pi-log-reset!)
      (%detach-handler!)
      (set! *log-dir-ready?* #f)
      (set! *log-dir-failed?* #f)
      (set! *log-dir* #f)
    ) ;define

    (define (%write severity message)
      (if (and (pi-log-enabled?) (pi-log-init!))
        (catch #t
          (lambda ()
            (log-set-level! (%log-level-from-env))
            (log-set-file-handler! (pi-log-file-path))
            (send-log severity message)
            (log-flush!)
            (%detach-handler!)
            #t
          ) ;lambda
          (lambda args (%detach-handler!) #f)
        ) ;catch
        #f
      ) ;if
    ) ;define

    (define (pi-log-error! message)
      (%write ERROR message)
    ) ;define

    (define (%alist-ref key alist default)
      (if (pair? alist)
        (let ((entry (assoc key alist)))
          (if (pair? entry) (cdr entry) default)
        ) ;let
        default
      ) ;if
    ) ;define

    (define (%as-string obj)
      (cond ((string? obj) obj)
            ((number? obj) (number->string obj))
            ((symbol? obj) (symbol->string obj))
            (else #f)
      ) ;cond
    ) ;define

    (define (%meta-field key value)
      (let ((str (%as-string value)))
        (if (and str (not (string-null? str)))
          (string-append " " (symbol->string key) "=" str)
          ""
        ) ;if
      ) ;let
    ) ;define

    (define (%bool-field key value)
      (%meta-field key (if value "yes" "no"))
    ) ;define

    ;; 记录一次异常 HTTP 请求：首行是请求上下文，随后原样记录响应体。
    ;;
    ;; info 为可选关联表，支持字段：
    ;;   provider model url attempt max-attempts retryable retry-after
    (define* (pi-log-http-error! status body (info '()))
      (let* ((status-str (%as-string (or status 0)))
             (body-str (if (string? body) body ""))
             (limit (pi-usage-limit-error? (or status 0) body-str (%alist-ref 'provider info #f))
             ) ;limit
             (kind (pi-usage-limit-kind limit))
             (attempt (%as-string (%alist-ref 'attempt info #f)))
             (max-attempts (%as-string (%alist-ref 'max-attempts info #f)))
             (attempt-str (if (and attempt max-attempts) (string-append attempt "/" max-attempts) attempt)
             ) ;attempt-str
             (head (string-append "http-error status="
                     status-str
                     (%meta-field 'provider (%alist-ref 'provider info #f))
                     (%meta-field 'model (%alist-ref 'model info #f))
                     (%meta-field 'attempt attempt-str)
                     (%bool-field 'retryable (%alist-ref 'retryable info #f))
                     (%meta-field 'limit kind)
                     (%meta-field 'retry-after (%alist-ref 'retry-after info #f))
                   ) ;string-append
             ) ;head
             (url-line
               (let ((url (%as-string (%alist-ref 'url info #f))))
                 (if url (string-append "\nurl=" url) "")
               ) ;let
             ) ;url-line
            ) ;
        (pi-log-error! (string-append head
                         url-line
                         "\n----- body begin -----\n"
                         (if (string-null? body-str) "(empty)" body-str)
                         "\n----- body end -----"
                       ) ;string-append
        ) ;pi-log-error!
      ) ;let*
    ) ;define*

  ) ;begin
) ;define-library
