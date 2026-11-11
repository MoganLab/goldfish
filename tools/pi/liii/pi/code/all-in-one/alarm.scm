;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code all-in-one alarm)
  (import (scheme base) (liii path) (liii string) (liii pi ai time))
  (export resolve-alarm-log-path pi-alarm-log!)
  (begin

    ;; resolve-alarm-log-path
    ;; 解析 alarm.log 完整文件路径。
    ;; 缺省或传入 #f 时返回默认路径 ~/.gf/agent/alarm.log，
    ;; 显式指定非空配置目录时返回该目录下的 alarm.log。
    (define (resolve-alarm-log-path . opt-config-dir)
      (let ((base-dir (if (and (pair? opt-config-dir) (car opt-config-dir))
                        (car opt-config-dir)
                        (path-join (path-home) ".gf" "agent")
                      ) ;if
            ) ;base-dir
           ) ;
        (path->string (path-join base-dir "alarm.log"))
      ) ;let
    ) ;define

    ;; pi-alarm-log!
    ;; 向 alarm.log 安全追加单行异常汇总记录。
    ;; 格式: [TIMESTAMP] [TYPE] provider=P model=M error=E message="MSG"
    (define (pi-alarm-log! log-type provider model err-code message . opt-config-dir)
      (let* ((log-file (apply resolve-alarm-log-path opt-config-dir))
             (parent (path-parent (path log-file)))
             (now-iso (pi-iso-8601-utc-now))
             (type-str (cond ((string? log-type) log-type)
                             ((symbol? log-type) (symbol->string log-type))
                             (else (object->string log-type))
                       ) ;cond
             ) ;type-str
             (prov-str (cond ((not provider) "none")
                             ((string? provider) provider)
                             ((symbol? provider) (symbol->string provider))
                             (else (object->string provider))
                       ) ;cond
             ) ;prov-str
             (model-str (cond ((not model) "none")
                              ((string? model) model)
                              ((symbol? model) (symbol->string model))
                              (else (object->string model))
                        ) ;cond
             ) ;model-str
             (err-str (if (number? err-code) (number->string err-code) (object->string err-code))
             ) ;err-str
             (msg-raw (cond ((not message) "")
                            ((string? message) message)
                            (else (object->string message))
                      ) ;cond
             ) ;msg-raw
             (msg-sanitized (string-replace msg-raw "\"" "'"))
             (line (string-append "[" now-iso "] [" type-str "] provider="
                     prov-str " model=" model-str " error=" err-str
                     " message=\"" msg-sanitized "\"\n"
                   ) ;string-append
             ) ;line
            ) ;
        (path-mkdir parent :parents #t :exist-ok #t)
        (path-append-text log-file line)
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
