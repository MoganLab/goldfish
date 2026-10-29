;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code modes interactive renderers)
  (export format-tool-call format-tool-call-segments format-tool-result-lines
    extract-result-text format-bash-tool-result-lines format-tool-duration
    get-tool-arg
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

    ;; 着色分段：角色与文本的 alist，角色为 symbol：
    ;;   bold=toolTitle 粗体 / accent=路径或模式 / muted=次要信息 / warning=范围
    ;; 对齐上游各 renderCall 渲染器（bash.ts/read.ts/grep.ts 等）的着色结构。

    ;; bash 格式化：整段 "$ cmd" 粗体，超时后缀 muted
    (define (format-bash-call-segments args)
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
        (if (string-null? timeout-suffix)
          (list (cons 'bold (string-append "$ " cmd-str)))
          (list (cons 'bold (string-append "$ " cmd-str)) (cons 'muted timeout-suffix))
        ) ;if
      ) ;let*
    ) ;define

    ;; read 范围后缀计算（对齐上游 formatReadLineRange：":起-止" / ":起"）
    (define (read-range-suffix offset limit)
      (cond
       ((and (number? offset) (number? limit))
        (string-append ":"
          (number->string offset)
          "-"
          (number->string (+ offset limit -1))
        ) ;string-append
       ) ;
       ((number? offset) (string-append ":" (number->string offset)))
       (else "")
      ) ;cond
    ) ;define

    ;; read 格式化：粗体名 + accent 路径 + warning 范围
    (define (format-read-call-segments args)
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
             (range (read-range-suffix offset limit))
            ) ;
        (if (string-null? range)
          (list (cons 'bold "read") (cons 'accent (string-append " " path)))
          (list (cons 'bold "read")
            (cons 'accent (string-append " " path))
            (cons 'warning range)
          ) ;list
        ) ;if
      ) ;let*
    ) ;define

    ;; 单文件操作工具（write/edit）格式化：粗体名 + accent 路径
    (define (format-file-call-segments tool-name args)
      (let ((path
              (cond ((string? args) args)
                    ((list? args)
                     (or (get-tool-arg "path" args #f) (get-tool-arg "file_path" args #f) ".")
                    ) ;
                    (else ".")
              ) ;cond
            ) ;path
           ) ;
        (list (cons 'bold tool-name) (cons 'accent (string-append " " path)))
      ) ;let
    ) ;define

    ;; 模式查找工具（grep/find）格式化：粗体名 + accent 模式 + muted " in 路径"
    ;; 对齐上游：grep 模式包裹为 /pattern/，find 模式原样；路径缺省为 "."
    (define (format-pattern-call-segments tool-name args)
      (let* ((pattern (if (list? args) (get-tool-arg "pattern" args "") ""))
             (path (if (list? args) (get-tool-arg "path" args #f) #f))
            ) ;
        (list (cons 'bold tool-name)
          (cons 'accent
            (string-append " "
              (if (string=? tool-name "grep") (string-append "/" pattern "/") pattern)
            ) ;string-append
          ) ;cons
          (cons 'muted (string-append " in " (if path path ".")))
        ) ;list
      ) ;let*
    ) ;define

    ;; ls 格式化：粗体名 + accent 路径
    (define (format-ls-call-segments args)
      (let ((path
              (cond ((string? args) args)
                    ((list? args) (or (get-tool-arg "path" args #f) "."))
                    (else ".")
              ) ;cond
            ) ;path
           ) ;
        (list (cons 'bold "ls") (cons 'accent (string-append " " path)))
      ) ;let
    ) ;define

    ;; web_search 格式化：粗体名 + accent 查询串
    (define (format-web-search-call-segments args)
      (let ((query (if (list? args) (get-tool-arg "query" args "") "")))
        (list (cons 'bold "web_search") (cons 'accent (string-append " \"" query "\"")))
      ) ;let
    ) ;define

    ;; 未知工具兜底：粗体名 + muted 参数预览（对齐上游 formatToolCallWithArgs）
    (define (format-generic-call-segments name args)
      (let ((summary (format-generic-args args)))
        (if (string-null? (string-trim-both summary))
          (list (cons 'bold name))
          (list (cons 'bold name) (cons 'muted (string-append " " summary)))
        ) ;if
      ) ;let
    ) ;define

    ;; 各内置工具的着色分段 (format-tool-call-segments name args)
    (define (format-tool-call-segments name args)
      (cond ((string=? name "bash") (format-bash-call-segments args))
            ((string=? name "read") (format-read-call-segments args))
            ((string=? name "write") (format-file-call-segments "write" args))
            ((string=? name "edit") (format-file-call-segments "edit" args))
            ((string=? name "grep") (format-pattern-call-segments "grep" args))
            ((string=? name "find") (format-pattern-call-segments "find" args))
            ((string=? name "ls") (format-ls-call-segments args))
            ((string=? name "web_search") (format-web-search-call-segments args))
            (else (format-generic-call-segments name args))
      ) ;cond
    ) ;define

    ;; 格式化工具调用命令行第一行的纯文本 (format-tool-call name args)
    ;; 即着色分段的文本拼接，供无色场景与降级换行使用
    (define (format-tool-call name args)
      (apply string-append
        (map (lambda (seg) (cdr seg)) (format-tool-call-segments name args))
      ) ;apply
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

    ;; Bash 输出折叠预览行数（对齐上游 BASH_PREVIEW_LINES = 5）
    (define %bash-preview-lines 5)

    ;; 折叠态尾部处理：只 wrap 最后 %bash-preview-lines 个逻辑行，
    ;; 取最后 %bash-preview-lines 个视觉行（每个逻辑行至少占 1 视觉行，
    ;; 因此最后 N 个视觉行必然来自最后 N 个逻辑行）。
    ;; 大输出每帧全文 wrap 会使 CPU 飙升，故折叠态绝不处理全文。
    ;; 注：hidden 计数为逻辑行数（上游为视觉行数，因其 wrap 为 C 实现；
    ;; 两者仅在超宽行 wrap 场景有差异，展示效果等价）
    (define (%collapsed-preview-lines clean width)
      (let* ((logical (string-split clean "\n"))
             (total (length logical))
             (tail (if (<= total %bash-preview-lines)
                     logical
                     (list-tail logical (- total %bash-preview-lines))
                   ) ;if
             ) ;tail
             (tail-visual (wrap-text (string-join tail "\n") width))
             (visual-total (length tail-visual))
             (shown (if (> visual-total %bash-preview-lines)
                      (list-tail tail-visual (- visual-total %bash-preview-lines))
                      tail-visual
                    ) ;if
             ) ;shown
             ;; 显示的逻辑行数：尾部全部显示，或被截到前 %bash-preview-lines 个逻辑行
             (shown-logical (if (> visual-total %bash-preview-lines) %bash-preview-lines (length tail))
             ) ;shown-logical
             (hidden (- total shown-logical))
            ) ;
        (if (> hidden 0)
          (cons (string-append "... ("
                  (number->string hidden)
                  " earlier lines, ctrl+o to expand)"
                ) ;string-append
            shown
          ) ;cons
          shown
        ) ;if
      ) ;let*
    ) ;define

    ;; 渲染层对齐上游 getTextOutput + Text：剥 ANSI/OSC、删控制字符与 \r，
    ;; TAB 展开为定宽 3 空格；展开态全文 wrap，折叠态仅尾部处理（见上）
    (define (format-bash-tool-result-lines result width expanded?)
      (let* ((raw (string-trim-both (extract-result-text result)))
             (clean
               (string-replace (string-replace (sanitize-binary-output (strip-ansi raw)) "\r" "")
                 "\t"
                 "   "
               ) ;string-replace
             ) ;clean
             (w (max 1 width))
            ) ;
        (cond ((string-null? clean) '())
              (expanded? (wrap-text clean w))
              (else (%collapsed-preview-lines clean w))
        ) ;cond
      ) ;let*
    ) ;define

    ;; 截断折叠辅助
    (define (truncate-lines name raw-lines max-lines)
      (let* ((total (length raw-lines)) (skipped (- total max-lines)))
        (if (string=? name "bash")
          (cons (string-append "... ("
                  (number->string skipped)
                  " earlier lines, ctrl+o to expand)"
                ) ;string-append
            (drop raw-lines skipped)
          ) ;cons
          (append (take raw-lines max-lines)
            (list (string-append "... ("
                    (number->string skipped)
                    " more lines, ctrl+o to expand)"
                  ) ;string-append
            ) ;list
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
    ;; max-lines 为 0 或 #f 时表示不折叠（展开态全文输出）
    (define* (format-tool-result-lines name result (max-lines 5))
      (if (or (not result) (and (string? result) (string-null? result)))
        '()
        (if (or (not max-lines) (and (number? max-lines) (= max-lines 0)))
          (let* ((res-str (extract-result-text result))
                 (res-trimmed (string-trim-right res-str #\newline))
                ) ;
            (if (string-null? res-trimmed) '() (string-split res-trimmed "\n"))
          ) ;let*
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
      ) ;if
    ) ;define*

  ) ;begin
) ;define-library
