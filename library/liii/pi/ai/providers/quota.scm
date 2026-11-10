;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 额度耗尽（quota / usage limit）识别的共享工具
;;
;; 具体判定由各 provider 模块注册的处理函数完成（见 providers/<id>.scm），
;; 这里只提供它们可以复用的结构化解析工具：
;;   - 从错误响应 JSON 中提取候选错误标识与官方消息
;;   - 候选标识与码表匹配
;;   - 结果结构 <pi-quota-limit>
;;   - 默认实现：状态码 {403,429} + 码表命中
;;
;; 业务标识字段在各家并不统一，候选顺序为：
;;   error.code（智谱）、code、error.name、type、error.type（kimi）

(define-library (liii pi ai providers quota)
  (import (scheme base) (scheme char) (liii string) (liii json))
  (export pi-quota-json-fields pi-quota-match-codes
    pi-quota-message-trailing-code pi-quota-error-message
    set-pi-quota-error-logger! %make-pi-quota-limit pi-quota-limit?
    pi-quota-limit-kind pi-quota-limit-code pi-quota-limit-message
    pi-quota-error-result
  ) ;export
  (begin

    ;; 「额度耗尽」判定结果
    ;;   kind    —— 额度窗口类型（five-hour / weekly / quota，由 provider 定义）
    ;;   code    —— 命中的错误标识（如 "1310"、"exceeded_current_quota_error"）
    ;;   message —— 服务商官方消息原文（可能 #f）
    (define-record-type <pi-quota-limit>
      (%make-pi-quota-limit kind code message)
      pi-quota-limit?
      (kind pi-quota-limit-kind)
      (code pi-quota-limit-code)
      (message pi-quota-limit-message)
    ) ;define-record-type

    ;; 判断是否为形如 ((k . v) ...) 的关联表
    (define (%alist? obj)
      (and (pair? obj)
        (let loop
          ((xs obj))
          (cond ((null? xs) #t)
                ((pair? (car xs)) (loop (cdr xs)))
                (else #f)
          ) ;cond
        ) ;let
      ) ;and
    ) ;define

    ;; JSON 对象取值：兼容字符串键与符号键
    (define (%val key obj default-val)
      (if (not (%alist? obj))
        default-val
        (let ((p (or (assoc key obj) (assoc (string->symbol key) obj))))
          (if (pair? p) (cdr p) default-val)
        ) ;let
      ) ;if
    ) ;define

    ;; 业务标识统一成字符串（服务商有的返回字符串，有的返回数字）
    (define (%as-code obj)
      (cond ((string? obj) (string-trim-both obj))
            ((number? obj) (number->string obj))
            ((symbol? obj) (symbol->string obj))
            (else #f)
      ) ;cond
    ) ;define

    (define (%as-message obj)
      (cond ((string? obj) obj)
            (else #f)
      ) ;cond
    ) ;define

    ;; 从错误响应体提取 (候选标识列表 . 官方消息)
    ;;
    ;;   {"error":{"code":"1310","message":"..."}}                 -> error.code（智谱）
    ;;   {"code":1310,"msg":"..."}                                 -> code
    ;;   {"error":{...,"type":"exceeded_current_quota_error",...}} -> error.type（kimi）
    ;;   {"type":"error","error":{"type":"rate_limit_error",...}}  -> type
    (define (pi-quota-json-fields body-str)
      (let ((parsed (catch #t (lambda () (string->json body-str)) (lambda args #f))))
        (if (not (%alist? parsed))
          (cons '() #f)
          (let* ((err (%val "error" parsed #f))
                 (err-obj (if (%alist? err) err #f))
                 (raw-candidates (list (%val "code" parsed #f)
                                   (%val "code" err-obj #f)
                                   (%val "name" err-obj #f)
                                   (%val "type" parsed #f)
                                   (%val "type" err-obj #f)
                                 ) ;list
                 ) ;raw-candidates
                 (codes
                   (let loop
                     ((rest raw-candidates) (acc '()))
                     (cond ((null? rest) (reverse acc))
                           ((%as-code (car rest)) (loop (cdr rest) (cons (%as-code (car rest)) acc)))
                           (else (loop (cdr rest) acc))
                     ) ;cond
                   ) ;let
                 ) ;codes
                 (msg
                   (%as-message (or (%val "message" parsed #f)
                                  (%val "msg" parsed #f)
                                  (%val "message" err-obj #f)
                                  (%val "msg" err-obj #f)
                                  (if (string? err) err #f)
                                ) ;or
                   ) ;%as-message
                 ) ;msg
                ) ;
            (cons codes msg)
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 候选标识与码表匹配：码表为 ((标识 . 额度类型) ...)，命中返回 (标识 . 额度类型)
    (define (pi-quota-match-codes codes table)
      (let loop
        ((rest codes))
        (cond ((null? rest) #f)
              ((assoc (car rest) table) => (lambda (entry) (cons (car rest) (cdr entry))))
              (else (loop (cdr rest)))
        ) ;cond
      ) ;let
    ) ;define

    ;; 从官方消息尾部提取形如 "(2067)" 的业务码（MiniMax 等把业务码附在
    ;; 消息末尾，没有独立的 code 字段）。数字码是稳定标识，不受文案改写与
    ;; 本地化影响；判定依据是码本身而不是文案，符合本模块的约定。
    (define (%ascii-digit? ch)
      (and (char>=? ch #\0) (char<=? ch #\9))
    ) ;define

    (define (pi-quota-message-trailing-code message)
      (if (not (string? message))
        #f
        (let* ((trimmed (string-trim-both message)) (len (string-length trimmed)))
          (if
            (or (= len 0) (not (char=? (string-ref trimmed (- len 1)) #\))))
            #f
            (let loop
              ((i (- len 2)) (acc '()))
              (cond ((< i 0) #f)
                    ((%ascii-digit? (string-ref trimmed i))
                     (loop (- i 1) (cons (string-ref trimmed i) acc))
                    ) ;
                    ((and (char=? (string-ref trimmed i) #\() (pair? acc)) (list->string acc))
                    (else #f)
              ) ;cond
            ) ;let
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    ;; 错误响应体解析异常的日志通道：由 (liii pi ai log) 初始化时注入
    ;; pi-log-error!。quota 层不能反向 import log（log -> limits -> registry
    ;; -> 各 provider -> quota，反向会循环依赖），故用回调注入；
    ;; 未注入时静默，不影响判定主流程。
    (define *quota-error-logger* #f)

    (define (set-pi-quota-error-logger! logger)
      (set! *quota-error-logger* logger)
    ) ;define

    ;; 日志用响应体片段：超长截断，避免畸形大 body 刷屏
    (define (%quota-body-snippet body-str)
      (if (not (string? body-str))
        ""
        (let ((n 120))
          (if (<= (string-length body-str) n)
            body-str
            (string-append (substring body-str 0 n) "...")
          ) ;if
        ) ;let
      ) ;if
    ) ;define

    ;; 从错误响应体安全提取 error.message：
    ;;   正常 -> message 字符串（可能为 '() 等其他类型，由调用方 string? 过滤）
    ;;   解析失败（非 JSON、畸形结构）-> 经注入的 logger 记录后返回 '()。
    ;; provider 的额度判定函数可直接复用，不必各自内联 catch。
    (define (pi-quota-error-message body-str)
      (catch #t
        (lambda () (json-ref (string->json body-str) "error" "message"))
        (lambda (tag . args)
          (if (procedure? *quota-error-logger*)
            (*quota-error-logger* (string-append "quota-parse-error"
                                    " tag="
                                    (if (symbol? tag) (symbol->string tag) "error")
                                    " body="
                                    (%quota-body-snippet body-str)
                                  ) ;string-append
            ) ;*quota-error-logger*
          ) ;if
          '()
        ) ;lambda
      ) ;catch
    ) ;define

    ;; 默认实现：状态码 ∈ {403, 429} 且业务标识命中码表
    ;;
    ;; provider 也可以不调用本函数，自行实现更复杂的判定（例如别的状态码、
    ;; 别的字段结构、或需要从结构化字段里读取重置时间）。
    (define (pi-quota-error-result status-code body-str table)
      (if (or (= status-code 429) (= status-code 403))
        (let* ((fields (pi-quota-json-fields body-str))
               (hit (pi-quota-match-codes (car fields) table))
              ) ;
          (if (pair? hit) (%make-pi-quota-limit (cdr hit) (car hit) (cdr fields)) #f)
        ) ;let*
        #f
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
