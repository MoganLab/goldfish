;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;; (liii logging): 与 SRFI-215 兼容的日志模块
;; 提供便捷函数、级别控制、格式化输出和文件处理器

(define-library (liii logging)
  (export send-log
    current-log-fields
    current-log-callback
    EMERGENCY
    ALERT
    CRITICAL
    ERROR
    WARNING
    NOTICE
    INFO
    DEBUG
    log-set-fields!
    log-set-callback!
    log-set-file-handler!
    current-log-fields-in-alist
    log-emergency
    log-alert
    log-critical
    log-error
    log-warning
    log-notice
    log-info
    log-debug
    log-set-level!
    current-log-level
    log-set-format!
    current-log-format
    log-flush!
    default-log-handler
    make-stdout-handler
    make-stderr-handler
    make-file-handler
    log-message-severity
    log-message-message
    log-message-field
  ) ;export
  (import (scheme base)
    (scheme write)
    (scheme time)
    (srfi srfi-13)
    (liii base)
    (liii error)
    (liii string)
    (liii path)
    (liii os)
  ) ;import
  (begin

    ;; ============== 日志级别常量 ==============
    (define EMERGENCY 0)
    (define ALERT 1)
    (define CRITICAL 2)
    (define ERROR 3)
    (define WARNING 4)
    (define NOTICE 5)
    (define INFO 6)
    (define DEBUG 7)

    ;; ============== 内部状态 ==============
    ;; 日志级别名称映射
    (define (severity->levelname severity)
      (case severity
       ((0) "EMERGENCY")
       ((1) "ALERT")
       ((2) "CRITICAL")
       ((3) "ERROR")
       ((4) "WARNING")
       ((5) "NOTICE")
       ((6) "INFO")
       ((7) "DEBUG")
       (else "UNKNOWN")
      ) ;case
    ) ;define

    ;; 内部状态变量
    (define *log-fields* '())
    (define *log-callback* (lambda (log-entry) (values)))
    (define *log-level* DEBUG)
    (define *log-format* "%(asctime)s [%(levelname)s] %(message)s")

    ;; 时间戳格式化
    (define (format-timestamp)
      (let ((t (g_datetime-now)))
        (string-append (number->string (vector-ref t 0))
          "-"
          (string-pad (number->string (vector-ref t 1)) 2 #\0)
          "-"
          (string-pad (number->string (vector-ref t 2)) 2 #\0)
          " "
          (string-pad (number->string (vector-ref t 3)) 2 #\0)
          ":"
          (string-pad (number->string (vector-ref t 4)) 2 #\0)
          ":"
          (string-pad (number->string (vector-ref t 5)) 2 #\0)
          "."
          (string-pad (number->string (quotient (vector-ref t 6) 1000)) 3 #\0)
        ) ;string-append
      ) ;let
    ) ;define

    ;; ============== plist / alist 转换 ==============
    (define (field-list->alist plist)
      (let f
        ((fields plist))
        (cond ((null? fields) '())
              ((or (not (pair? fields)) (not (pair? (cdr fields))))
               (error "short field list" plist)
              ) ;
              (else (let ((k (car fields)) (v (cadr fields)))
                      (if (not v)
                        (f (cddr fields))
                        (let ((k^ (cond ((symbol? k) k) (else (error "invalid key" k plist))))
                              (v^ (cond ((string? v) v)
                                        ((and (integer? v) (exact? v)) v)
                                        ((bytevector? v) v)
                                        (else (let ((p (open-output-string))) (write v p) (get-output-string p)))
                                  ) ;cond
                              ) ;v^
                             ) ;
                          (cons (cons k^ v^) (f (cddr fields)))
                        ) ;let
                      ) ;if
                    ) ;let
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (alist->plist alist)
      (let loop
        ((a alist) (result '()))
        (if (null? a)
          (reverse result)
          (loop (cdr a) (cons (cdar a) (cons (caar a) result)))
        ) ;if
      ) ;let
    ) ;define

    ;; ============== SRFI-215 兼容层 ==============
    (define (current-log-fields)
      *log-fields*
    ) ;define

    (define (current-log-callback)
      *log-callback*
    ) ;define

    (define (send-log severity message . plist)
      (unless (and (exact? severity) (integer? severity) (<= 0 severity 7))
        (error 'wrong-type-arg
          "send-log: expected a severity from 0 to 7"
          severity
          message
          plist
        ) ;error
      ) ;unless
      (unless (string? message)
        (error 'wrong-type-arg
          "send-log: expected message to be a string"
          severity
          message
          plist
        ) ;error
      ) ;unless
      ;; 级别过滤
      (when (>= *log-level* severity)
        (let* ((fields (append plist *log-fields*))
               (alist (field-list->alist fields))
               (callback *log-callback*)
              ) ;
          (if callback
            (callback `((SEVERITY unquote severity)
                        (MESSAGE unquote message)
                        ,@alist))
            ;; 默认行为：ERROR 及以上到 stderr，其他到 stdout
            (let ((port (if (<= severity ERROR) (current-error-port) (current-output-port))))
              (default-log-handler `((SEVERITY unquote severity)
                                     (MESSAGE unquote message)
                                     ,@alist)
                port
              ) ;default-log-handler
            ) ;let
          ) ;if
        ) ;let*
      ) ;when
    ) ;define

    ;; ============== 设置函数 ==============
    (define (log-set-fields! plist)
      (set! *log-fields* plist)
    ) ;define

    (define (log-set-callback! callback)
      (set! *log-callback* callback)
    ) ;define

    (define (current-log-fields-in-alist alist)
      (set! *log-fields* (alist->plist alist))
    ) ;define

    ;; ============== 便捷函数 ==============
    (define (log-emergency message . args)
      (apply send-log
        EMERGENCY
        (if (null? args) message (apply pyfmt message args))
        args
      ) ;apply
    ) ;define

    (define (log-alert message . args)
      (apply send-log ALERT (if (null? args) message (apply pyfmt message args)) args)
    ) ;define

    (define (log-critical message . args)
      (apply send-log
        CRITICAL
        (if (null? args) message (apply pyfmt message args))
        args
      ) ;apply
    ) ;define

    (define (log-error message . args)
      (apply send-log ERROR (if (null? args) message (apply pyfmt message args)) args)
    ) ;define

    (define (log-warning message . args)
      (apply send-log
        WARNING
        (if (null? args) message (apply pyfmt message args))
        args
      ) ;apply
    ) ;define

    (define (log-notice message . args)
      (apply send-log
        NOTICE
        (if (null? args) message (apply pyfmt message args))
        args
      ) ;apply
    ) ;define

    (define (log-info message . args)
      (apply send-log INFO (if (null? args) message (apply pyfmt message args)) args)
    ) ;define

    (define (log-debug message . args)
      (apply send-log DEBUG (if (null? args) message (apply pyfmt message args)) args)
    ) ;define

    ;; ============== 日志级别控制 ==============
    (define (log-set-level! severity)
      (set! *log-level* severity)
    ) ;define

    (define (current-log-level)
      *log-level*
    ) ;define

    ;; ============== 格式控制 ==============
    (define (log-set-format! format-string)
      (set! *log-format* format-string)
    ) ;define

    (define (current-log-format)
      *log-format*
    ) ;define

    ;; ============== 默认处理器 ==============
    (define (default-log-handler message port)
      (let* ((severity (cdr (assq 'SEVERITY message)))
             (msg (cdr (assq 'MESSAGE message)))
             (formatted (pyfmt *log-format*
                          'asctime
                          (format-timestamp)
                          'levelname
                          (severity->levelname severity)
                          'severity
                          severity
                          'message
                          msg
                        ) ;pyfmt
             ) ;formatted
            ) ;
        (display formatted port)
        (newline port)
      ) ;let*
    ) ;define

    ;; ============== stdout / stderr 处理器 ==============
    (define (make-stdout-handler)
      (lambda (msg) (default-log-handler msg (current-output-port)))
    ) ;define

    (define (make-stderr-handler)
      (lambda (msg) (default-log-handler msg (current-error-port)))
    ) ;define

    ;; ============== 文件处理器 ==============
    (define (make-file-handler path)
      ;; 自动创建父目录
      (let ((parent (path-parent path)))
        (when (and parent (not (path-exists? parent)))
          (mkdir (path->string parent))
        ) ;when
      ) ;let
      ;; 返回带缓冲的回调函数
      (let ((buffer '()) (buffer-size 0) (max-buffer-size 4096) (max-buffer-count 10))
        (define (flush-buffer)
          (when (not (null? buffer))
            (let ((port (open-output-file (path->string path) 'append)))
              (for-each (lambda (line) (display line port) (newline port)) (reverse buffer))
              (close-output-port port)
            ) ;let
            (set! buffer '())
            (set! buffer-size 0)
          ) ;when
        ) ;define
        (lambda (msg)
          (let* ((severity (cdr (assq 'SEVERITY msg)))
                 (line (pyfmt *log-format*
                         'asctime
                         (format-timestamp)
                         'levelname
                         (severity->levelname severity)
                         'severity
                         severity
                         'message
                         (cdr (assq 'MESSAGE msg))
                       ) ;pyfmt
                 ) ;line
                ) ;
            (set! buffer (cons line buffer))
            (set! buffer-size (+ buffer-size (string-length line)))
            (when (or (>= (length buffer) max-buffer-count) (>= buffer-size max-buffer-size))
              (flush-buffer)
            ) ;when
          ) ;let*
        ) ;lambda
      ) ;let
    ) ;define

    ;; ============== log-flush! ==============
    (define (log-flush!)
      ;; 对于文件处理器，这里需要一种方式来刷新当前回调的缓冲
      ;; 当前实现：如果回调是文件处理器，则无法直接访问其内部 flush-buffer
      ;; 简化处理：不支持直接刷新文件 handler 的缓冲
      (values)
    ) ;define

    ;; ============== 辅助函数 ==============
    (define (log-message-severity msg)
      (cdr (assq 'SEVERITY msg))
    ) ;define

    (define (log-message-message msg)
      (cdr (assq 'MESSAGE msg))
    ) ;define

    (define (log-message-field msg key)
      (let ((pair (assq key msg)))
        (and pair (cdr pair))
      ) ;let
    ) ;define

    ;; ============== log-set-file-handler! ==============
    (define (log-set-file-handler! path)
      (set! *log-callback* (make-file-handler path))
    ) ;define

  ) ;begin
) ;define-library
