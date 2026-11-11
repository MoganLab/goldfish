;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; all-in-one 调度异常记录：写入 (liii pi ai log) 的按天日志文件
;; <agent>/logs/pi-YYYY-MM-DD.log，时间戳为机器本地时区。
;; 不再单独维护 alarm.log 文件。
(define-library (liii pi code all-in-one alarm)
  (import (scheme base)
    (liii path)
    (liii string)
    (liii date-time)
    (liii pi ai log)
  ) ;import
  (export resolve-alarm-log-path pi-alarm-log!)
  (begin

    ;; resolve-alarm-log-path
    ;; 返回当前生效的按天日志完整路径。
    ;; 未传配置目录时使用用户配置目录 ~/.gf/agent/logs；
    ;; 传入非空配置目录时使用该目录下的 logs/。
    (define (resolve-alarm-log-path . opt-config-dir)
      (let ((dir
              (if (and (pair? opt-config-dir) (car opt-config-dir))
                (path-join (path->string (car opt-config-dir)) "logs")
                (pi-log-dir)
              ) ;if
            ) ;dir
           ) ;
        (path->string
          (path-join dir
            (string-append "pi-"
              (date->iso-8601 (timestamp-date (current-system-timestamp)))
              ".log"
            ) ;string-append
          ) ;path-join
        ) ;path->string
      ) ;let
    ) ;define

    (define (%type-str log-type)
      (cond ((string? log-type) log-type)
            ((symbol? log-type) (symbol->string log-type))
            (else (object->string log-type))
      ) ;cond
    ) ;define

    (define (%or-none value)
      (cond ((not value) "none")
            ((string? value) value)
            ((symbol? value) (symbol->string value))
            (else (object->string value))
      ) ;cond
    ) ;define

    ;; pi-alarm-log!
    ;; 追加一条异常汇总记录到当天的 pi-<date>.log。
    ;; 格式: [TIMESTAMP] [TYPE] provider=P model=M error=E message="MSG"
    (define (pi-alarm-log! log-type provider model err-code message . opt-config-dir)
      (let* ((log-file (apply resolve-alarm-log-path opt-config-dir))
             (parent (path-parent (path log-file)))
             ;; 机器本地时区，带显式 UTC 偏移；UTC 环境输出 Z。
             (now-iso (timestamp->iso-8601 (current-system-timestamp)))
             (err-str (if (number? err-code) (number->string err-code) (object->string err-code))
             ) ;err-str
             (msg-raw (cond ((not message) "")
                            ((string? message) message)
                            (else (object->string message))
                      ) ;cond
             ) ;msg-raw
             (msg-sanitized (string-replace msg-raw "\"" "'"))
             (line (string-append "["
                     now-iso
                     "] ["
                     (%type-str log-type)
                     "] provider="
                     (%or-none provider)
                     " model="
                     (%or-none model)
                     " error="
                     err-str
                     " message=\""
                     msg-sanitized
                     "\"\n"
                   ) ;string-append
             ) ;line
            ) ;
        (path-mkdir parent :parents #t :exist-ok #t)
        (path-append-text log-file line)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
