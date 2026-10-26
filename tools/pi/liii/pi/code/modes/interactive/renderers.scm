;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive renderers)
  (export format-tool-call format-tool-result-lines extract-result-text
    format-bash-tool-result-lines format-tool-duration get-tool-arg
  ) ;export
  (import (scheme base)
    (liii string)
    (liii list)
    (liii tui text-width)
    (liii pi code utils shell)
  ) ;import
  (begin

    ;; 判断键是否匹配 (兼容 string 与 symbol)
    (define (key-matches? entry-key target-key)
      (or (equal? entry-key target-key)
        (and (symbol? target-key)
          (string? entry-key)
          (string=? (symbol->string target-key) entry-key)
        ) ;and
        (and (string? target-key)
          (symbol? entry-key)
          (string=? target-key (symbol->string entry-key))
        ) ;and
      ) ;or
    ) ;define

    ;; 安全获取 alist 中的参数
    (define* (get-tool-arg key args (default #f))
      (if (not (list? args))
        default
        (let ((pair (find (lambda (p) (and (pair? p) (key-matches? (car p) key))) args)))
          (if pair (cdr pair) default)
        ) ;let
      ) ;if
    ) ;define*

    ;; 格式化单个键值对
    (define (format-kv-pair pair)
      (if (pair? pair)
        (let ((k-str
                (cond ((symbol? (car pair)) (symbol->string (car pair)))
                      ((string? (car pair)) (car pair))
                      (else (object->string (car pair)))
                ) ;cond
              ) ;k-str
              (v-str
                (cond ((symbol? (cdr pair)) (symbol->string (cdr pair)))
                      ((string? (cdr pair)) (cdr pair))
                      (else (object->string (cdr pair)))
                ) ;cond
              ) ;v-str
             ) ;
          (string-append k-str "=" v-str)
        ) ;let
        (object->string pair)
      ) ;if
    ) ;define

    ;; 格式化未知工具参数为简短 key=value
    (define (format-generic-args args)
      (cond ((string? args) args)
            ((list? args) (string-join (map format-kv-pair args) " "))
            (else (object->string args))
      ) ;cond
    ) ;define

    ;; bash 格式化
    (define (format-bash-call args)
      (let* ((cmd (cond ((string? args) args)
                        ((list? args) (get-tool-arg "command" args))
                        (else #f)
                  ) ;cond
             ) ;cmd
             (timeout (if (list? args) (get-tool-arg "timeout" args #f) #f))
             (cmd-str
               (if
                 (and cmd (not (string-null? (string-trim-both cmd))))
                 cmd
                 "..."
               ) ;if
             ) ;cmd-str
             (timeout-suffix (if (number? timeout)
                               (string-append " (timeout " (number->string timeout) "s)")
                               ""
                             ) ;if
             ) ;timeout-suffix
            ) ;
        (string-append "$ " cmd-str timeout-suffix)
      ) ;let*
    ) ;define

    ;; read 范围后缀计算
    (define (read-range-suffix offset limit)
      (cond
       ((and (number? offset) (number? limit))
        (string-append " [L"
          (number->string offset)
          "-"
          (number->string (+ offset limit -1))
          "]"
        ) ;string-append
       ) ;
       ((number? offset) (string-append " [L" (number->string offset) "+]"))
       ((number? limit) (string-append " [limit " (number->string limit) "]"))
       (else "")
      ) ;cond
    ) ;define

    ;; read 格式化
    (define (format-read-call args)
      (let* ((path
               (cond ((string? args) args)
                     ((list? args)
                      (or (get-tool-arg "path" args #f) (get-tool-arg "file_path" args #f) ".")
                     ) ;
                     (else ".")
               ) ;cond
             ) ;path
             (offset (if (list? args) (get-tool-arg "offset" args #f) #f))
             (limit (if (list? args) (get-tool-arg "limit" args #f) #f))
            ) ;
        (string-append "read " path (read-range-suffix offset limit))
      ) ;let*
    ) ;define

    ;; 单文件操作工具（write/edit）格式化
    (define (format-file-call tool-name args)
      (let ((path
              (cond ((string? args) args)
                    ((list? args)
                     (or (get-tool-arg "path" args #f) (get-tool-arg "file_path" args #f) ".")
                    ) ;
                    (else ".")
              ) ;cond
            ) ;path
           ) ;
        (string-append tool-name " " path)
      ) ;let
    ) ;define

    ;; 模式查找工具（grep/find）格式化
    (define (format-pattern-call tool-name args)
      (let* ((pattern (if (list? args) (get-tool-arg "pattern" args "") ""))
             (path (if (list? args) (get-tool-arg "path" args #f) #f))
            ) ;
        (if path
          (string-append tool-name " \"" pattern "\" " path)
          (string-append tool-name " \"" pattern "\"")
        ) ;if
      ) ;let*
    ) ;define

    ;; ls 格式化
    (define (format-ls-call args)
      (let ((path
              (cond ((string? args) args)
                    ((list? args) (or (get-tool-arg "path" args #f) "."))
                    (else ".")
              ) ;cond
            ) ;path
           ) ;
        (string-append "ls " path)
      ) ;let
    ) ;define

    ;; web_search 格式化
    (define (format-web-search-call args)
      (let ((query (if (list? args) (get-tool-arg "query" args "") "")))
        (string-append "web_search \"" query "\"")
      ) ;let
    ) ;define

    ;; 格式化工具调用命令行第一行 (format-tool-call tool-name args)
    (define (format-tool-call name args)
      (cond ((string=? name "bash") (format-bash-call args))
            ((string=? name "read") (format-read-call args))
            ((string=? name "write") (format-file-call "write" args))
            ((string=? name "edit") (format-file-call "edit" args))
            ((string=? name "grep") (format-pattern-call "grep" args))
            ((string=? name "find") (format-pattern-call "find" args))
            ((string=? name "ls") (format-ls-call args))
            ((string=? name "web_search") (format-web-search-call args))
            (else
              (let ((summary (format-generic-args args)))
                (if (string-null? (string-trim-both summary))
                  name
                  (string-append name " " summary)
                ) ;if
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; 时长格式与上游 formatToolDuration 一致。
    (define (format-tool-duration ms)
      (let ((seconds (/ ms 1000.0)))
        (if (< seconds 60)
          (let* ((tenths (inexact->exact (round (* seconds 10))))
                 (whole (quotient tenths 10))
                 (digit (remainder tenths 10))
                ) ;
            (string-append (number->string whole) "." (number->string digit) "s")
          ) ;let*
          (let* ((total-seconds (inexact->exact (floor seconds)))
                 (minutes (quotient total-seconds 60))
                 (remainder-seconds (remainder total-seconds 60))
                ) ;
            (if (< minutes 60)
              (string-append (number->string minutes)
                "m "
                (number->string remainder-seconds)
                "s"
              ) ;string-append
              (string-append (number->string (quotient minutes 60))
                "h "
                (number->string (remainder minutes 60))
                "m "
                (number->string remainder-seconds)
                "s"
              ) ;string-append
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; Bash 输出折叠以视觉行为单位：对齐上游 getTextOutput + Text 组件，
    ;; 先剥 ANSI（CSI+OSC）、删控制字符与 \r，再把 TAB 展开为定宽 3 空格
    ;; （防止终端制表位导致卡片背景错位），最后按当前宽度换行。
    ;; 折叠时保留末尾 5 行，与上游 VisualLinePreview 一致。
    (define (format-bash-tool-result-lines result width expanded?)
      (let* ((raw (string-trim-both (extract-result-text result)))
             (clean
               (string-replace (string-replace (sanitize-binary-output (strip-ansi raw)) "\r" "")
                 "\t"
                 "   "
               ) ;string-replace
             ) ;clean
             (all-lines (if (string-null? clean) '() (wrap-text clean (max 1 width))))
             (total (length all-lines))
            ) ;
        (cond ((null? all-lines) '())
              ((or expanded? (<= total 5)) all-lines)
              (else
                (let ((hidden (- total 5)))
                  (cons (string-append "... ("
                          (number->string hidden)
                          " earlier lines, ctrl+o to expand)"
                        ) ;string-append
                    (drop all-lines hidden)
                  ) ;cons
                ) ;let
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define

    ;; 截断折叠辅助
    (define (truncate-lines name raw-lines max-lines)
      (let* ((total (length raw-lines)) (skipped (- total max-lines)))
        (if (string=? name "bash")
          (cons (string-append "... (" (number->string skipped) " earlier lines)")
            (drop raw-lines skipped)
          ) ;cons
          (append (take raw-lines max-lines)
            (list (string-append "... (" (number->string skipped) " more lines)"))
          ) ;append
        ) ;if
      ) ;let*
    ) ;define

    ;; 从多种可能的工具返回结果结构中提取纯文本内容
    (define (extract-result-text res)
      (cond ((string? res) res)
            ((not res) "")
            ((vector? res)
             (if (> (vector-length res) 0) (extract-result-text (vector-ref res 0)) "")
            ) ;
            ((pair? res)
             (let ((t (or (assoc 'text res) (assoc "text" res))))
               (if t
                 (if (pair? t) (cdr t) (object->string t))
                 (let ((c (or (assoc 'content res) (assoc "content" res))))
                   (if c (extract-result-text (cdr c)) (object->string res))
                 ) ;let
               ) ;if
             ) ;let
            ) ;
            ((let ((c (catch #t (lambda () (res 'content)) (lambda args #f))))
               (and c (extract-result-text c))
             ) ;let
            ) ;
            ((let ((t (catch #t (lambda () (res 'text)) (lambda args #f))))
               (and t (extract-result-text t))
             ) ;let
            ) ;
            (else (object->string res))
      ) ;cond
    ) ;define

    ;; 格式化工具结果行并折叠 (format-tool-result-lines name result :max-lines 5)
    (define* (format-tool-result-lines name result (max-lines 5))
      (if (or (not result) (and (string? result) (string-null? result)))
        '()
        (let* ((res-str (extract-result-text result))
               (res-trimmed (string-trim-right res-str #\newline))
               (raw-lines (string-split res-trimmed "\n"))
              ) ;
          (if (<= (length raw-lines) max-lines)
            raw-lines
            (truncate-lines name raw-lines max-lines)
          ) ;if
        ) ;let*
      ) ;if
    ) ;define*

  ) ;begin
) ;define-library
