(define-library (liii goldfix)
  (import (scheme base))
  (import (liii sys))
  (import (liii string))
  (import (liii error))
  (import (liii ascii))
  (import (liii goldfix-raw-string))
  (import (liii goldfix-scheme))
  (import (liii goldfix-lint))
  (import (liii goldfix-paren))
  (import (liii goldfix-file))
  (import (liii goldfix-env))
  (import (liii goldfix-split))
  (import (liii goldfix-line-scan))
  (import (liii goldfix-list))
  (import (liii list))
  (export main)
  (export fix-content)
  (export normalize-multi-open-lines)

  (begin
    ;; 读取文件原始内容，保留文件末尾换行
    (define (read-file-exact-content file-path)
      (call-with-input-file file-path
        (lambda (port)
          (let loop ((chars '()))
            (let ((ch (read-char port)))
              (if (eof-object? ch)
                (list->string (reverse chars))
                (loop (cons ch chars))
              ) ;if
            ) ;let
          ) ;let
        ) ;lambda
      ) ;call-with-input-file
    ) ;define

    ;; 检查内容是否能被 reader 完整读入
    (define (content-readable? content)
      (guard (ex (else #f))
        (let ((readable? #f))
          (call-with-input-string content
            (lambda (port)
              (let loop ()
                (let ((form (read port)))
                  (if (eof-object? form)
                    (set! readable? #t)
                    (loop)
                  ) ;if
                ) ;let
              ) ;let
            ) ;lambda
          ) ;call-with-input-string
          readable?
        ) ;let
      ) ;guard
    ) ;define

    ;; 内容健康的最低标准：括号整体平衡
    (define (content-healthy? lines)
      (check-lines-balanced lines)
    ) ;define

    (define (right-tag-line? line)
      (let ((trimmed (string-trim line)))
        (and (not (string-null? trimmed))
             (ascii-right-paren? (string-ref trimmed 0))
        ) ;and
      ) ;let
    ) ;define


    (define (preferred-tag-from-tokens tokens)
      (let ((meaningful tokens))
        (cond
          ((null? meaningful) "")
          ((null? (cdr meaningful)) (car meaningful))
          (else
           (let loop ((rest meaningful))
             (cond
               ((null? rest)
                (car (reverse meaningful))
               ) ;
               ((or (string-index (car rest) #\-)
                    (string-index (car rest) #\*))
                (car rest)
               ) ;
               (else
                (loop (cdr rest))
               ) ;else
             ) ;cond
           ) ;let
          ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (canonicalize-right-tag-line line)
      (if (not (right-tag-line? line))
        line
        (let* ((trimmed (string-trim line))
               (col (- (string-length line) (string-length trimmed)))
               (indent (make-string col #\space))
               (tokens (extract-identifier-tokens trimmed))
               (tag (preferred-tag-from-tokens tokens)))
          (if (string-null? tag)
            (string-append indent ") ;")
            (string-append indent ") ;" tag)
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    (define (line-ends-with-rparen? line)
      (let ((trimmed (string-trim-right line)))
        (and (not (string-null? trimmed))
             (char=? (string-ref trimmed (- (string-length trimmed) 1)) #\))
        ) ;and
      ) ;let
    ) ;define

    (define (inline-right-tag-comment? comment-part)
      (and (not (string-null? comment-part))
           (not (char-whitespace? (string-ref comment-part 0)))
      ) ;and
    ) ;define

    (define (strip-inline-right-tag-comment line env-tags)
      (let* ((comment-start (line-comment-start-index line))
             (len (string-length line)))
        (if (or (= comment-start len)
                (= comment-start 0))
          line
          (let*
            ((code-part (string-trim-right (substring line 0 comment-start)))
             (comment-part (substring line (+ comment-start 1) len))
             (tag
               (preferred-tag-from-tokens
                 (extract-identifier-tokens comment-part))
               ) ;preferred-tag-from-tokens
             ) ;tag
            (if (and (not (string-null? code-part))
                     (line-ends-with-rparen? code-part)
                     (not (right-tag-line? line))
                     (inline-right-tag-comment? comment-part)
                     (not (string-null? tag))
                     (member tag env-tags))
              code-part
              line
            ) ;if
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    (define (repairable-semicolon-right-tag-line? line previous-line env-tags)
      (let ((trimmed (string-trim line)))
        (and previous-line
             (line-ends-with-rparen? previous-line)
             (not (string-null? trimmed))
             (char=? (string-ref trimmed 0) #\;)
             (or (= (string-length trimmed) 1)
                 (and (not (char=? (string-ref trimmed 1) #\;))
                      (not (char-whitespace? (string-ref trimmed 1)))
                 ) ;and
             ) ;or
             (let
               ((tag
                  (preferred-tag-from-tokens
                    (extract-identifier-tokens
                      (substring trimmed 1 (string-length trimmed)))
                    ) ;extract-identifier-tokens
                  ) ;preferred-tag-from-tokens
               ) ;
               (and (not (string-null? tag))
                    (member tag env-tags)
               ) ;and
             ) ;let
        ) ;and
      ) ;let
    ) ;define

    (define (canonicalize-right-tag-lines lines)
      (let*
        ((env-tags (map env-tag (scan-environments lines)))
         (normalized-inline-comments
           (map
             (lambda (line)
               (strip-inline-right-tag-comment line env-tags)
             ) ;lambda
             lines
           ) ;map
         ) ;normalized-inline-comments
         (normalized-env-tags (map env-tag (scan-environments normalized-inline-comments)))
        ) ;
        (let loop ((remaining lines)
                   (normalized-remaining normalized-inline-comments)
                   (block-depth 0)
                   (in-string #f)
                   (escape-next #f)
                   (previous-line #f)
                   (result '()))
          (if (null? normalized-remaining)
            (reverse result)
            (let*
              ((line (car normalized-remaining))
               (trimmed (string-trim line))
               (normalized
                (cond
                  ((or (> block-depth 0) in-string escape-next)
                   line
                  ) ;
                  ((right-tag-line? line)
                   (canonicalize-right-tag-line line)
                  ) ;
                  ((repairable-semicolon-right-tag-line? line previous-line normalized-env-tags)
                   (let*
                     ((col (- (string-length line) (string-length trimmed)))
                      (indent (make-string col #\space))
                      (tag
                        (preferred-tag-from-tokens
                          (extract-identifier-tokens
                            (substring trimmed 1 (string-length trimmed))
                          ) ;extract-identifier-tokens
                        ) ;preferred-tag-from-tokens
                      ) ;tag
                     ) ;
                     (if (string-null? tag)
                       (string-append indent ") ;")
                       (string-append indent ") ;" tag)
                     ) ;if
                   ) ;let*
                  ) ;
                  (else
                   line
                  ) ;else
                ) ;cond
               ) ;normalized
              ) ;
              (let-values (((_paren-counts next-block-depth next-in-string next-escape-next)
                            (count-parens-with-state normalized
                                                     block-depth
                                                     in-string
                                                     escape-next)
                            ) ;count-parens-with-state
              ) ;let-values
                (loop (cdr remaining)
                      (cdr normalized-remaining)
                      next-block-depth
                      next-in-string
                      next-escape-next
                      normalized
                      (cons normalized result)
                ) ;loop
            ) ;
            ) ;let*
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (same-env-origin? a b)
      (and (string=? (env-tag a) (env-tag b))
           (= (env-lparen-line a) (env-lparen-line b))
           (= (env-lparen-col a) (env-lparen-col b))
      ) ;and
    ) ;define

    (define (find-corresponding-env env envs)
      (let loop ((rest envs))
        (if (null? rest)
          #f
          (if (same-env-origin? env (car rest))
            (car rest)
            (loop (cdr rest))
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (insert-remaining-right-tags lines reference-lines)
      (let*
        ((envs (scan-environments lines))
         (reference-envs (scan-environments reference-lines))
         (to-insert
           (filter
             (lambda (env)
               (let ((ref-env (find-corresponding-env env reference-envs)))
                 (and ref-env
                      (env-rparen-line ref-env))
                 ) ;and
               ) ;let
             (sort-envs-for-insertion envs lines)
     ) ;filter
         ) ;to-insert
         (original-total (length lines))
        ) ;
        (let loop ((remaining to-insert)
                   (current-lines lines))
          (if (null? remaining)
            current-lines
            (let*
              ((env (car remaining))
               (raw-pos (find-insert-position env envs original-total current-lines))
               (pos (cond
                      ((number? raw-pos) raw-pos)
                      ((number? (env-lparen-line env)) (env-lparen-line env))
                      (else 0))
               ) ;pos
               (tag-line (make-right-tag-line env))
               (new-lines (insert-line-at current-lines pos tag-line))
              ) ;
              (env-set-rparen-line! env (+ pos 1))
              (loop (cdr remaining) new-lines)
            ) ;let*
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (first-healthy-lines candidates)
      (let loop ((rest candidates))
        (if (null? rest)
          #f
          (if (content-healthy? (car rest))
            (car rest)
            (loop (cdr rest))
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (define-function-header-line? line)
      (let* ((trimmed (string-trim line))
             (len (string-length trimmed)))
        (and (> len 8)
             (char=? (string-ref trimmed 0) #\()
             (string=? (extract-tag trimmed) "define")
             (let loop ((i 7))
               (cond
                 ((>= i len) #f)
                 ((char-whitespace? (string-ref trimmed i))
                  (loop (+ i 1))
                 ) ;
                 (else
                  (char=? (string-ref trimmed i) #\()
                 ) ;else
               ) ;cond
             ) ;let
        ) ;and
      ) ;let*
    ) ;define

    ;; 如果累计余额会跌到负数，说明当前行尾部出现了真正多余的右括号。
    ;; 这里只修剪尾部那部分多余的 )，避免碰到中间仍有后续代码的场景。
    (define (trim-excess-trailing-rparens lines)
      (let loop ((remaining lines)
                 (accum 0)
                 (state initial-line-start-state)
                 (result '()))
        (if (null? remaining)
          (reverse result)
          (let*
            ((line (car remaining))
             (next-state (advance-line-start-state line state))
             (diff (line-net-open-count-at-state line state))
             (new-accum (+ accum diff))
             (touches-raw-string?
              (line-touches-raw-string-at-state? line state)
             ) ;touches-raw-string?
            ) ;
            (if (< new-accum 0)
              (let ((remove-count (- 0 new-accum)))
                (if (or (right-tag-line? line)
                        touches-raw-string?)
                  ;; 显式 right tag 先保留给后续结构修复阶段处理；
                  ;; raw-string 相关行等后续阶段接入 stateful line-scan 后再精修。
                  (loop (cdr remaining)
                        0
                        next-state
                        (cons line result)
                  ) ;loop
                  (let ((trimmed-line (remove-rparens-from-right-by-diff line remove-count)))
                    (loop (cdr remaining)
                          0
                          next-state
                          (cons trimmed-line result)
                    ) ;loop
                  ) ;let
                ) ;if
              ) ;let
              (loop (cdr remaining)
                    new-accum
                    next-state
                    (cons line result)
              ) ;loop
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 多行 define 的头部应只留下外层 define 自身未闭合的一个左括号。
    ;; 若头部净 open 数大于 1，通常表示函数签名缺少了一个 )。
    (define (repair-define-header-lines lines)
      (let* ((normalized-lines (trim-excess-trailing-rparens lines))
             (line-start-states (compute-line-start-states normalized-lines))
             (details (scan-environment-details normalized-lines)))
        (let loop ((remaining details)
                   (current-lines normalized-lines))
          (if (null? remaining)
            current-lines
            (let* ((detail (car remaining))
                   (env (env-detail-env detail))
                   (lparen-line (env-lparen-line env))
                   (rparen-line (env-rparen-line env)))
              (if (and (string=? (env-tag env) "define")
                       rparen-line
                       (> rparen-line lparen-line))
                (let* ((line-idx (- lparen-line 1))
                       (line (list-ref current-lines line-idx))
                       (line-state (list-ref line-start-states line-idx))
                       (net-open-count (line-net-open-count-at-state line line-state)))
                  (if (and (> net-open-count 1)
                           (define-function-header-line? line))
                    (let* ((fixed-line (add-rparens-by-diff line (- net-open-count 1)))
                           (new-lines (list-set current-lines line-idx fixed-line)))
                      (loop (cdr remaining) new-lines)
                    ) ;let*
                    (loop (cdr remaining) current-lines)
                  ) ;if
                ) ;let*
                (loop (cdr remaining) current-lines)
              ) ;if
            ) ;let*
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    ;; 运行核心修复流程，返回修复后的行列表
    (define (run-goldfix-lines lines)
      (let*
        ((prepared-lines (repair-define-header-lines lines))
         (normalized-lines (normalize-multi-open-lines prepared-lines))
         ;; 插入右标记
         (lines-with-right-tags (insert-single-line-of-right-tag normalized-lines))
         (envs2 (scan-environments lines-with-right-tags))
         ;; 移除无主右括号行
         (cleaned-lines (remove-orphan-right-paren-lines lines-with-right-tags envs2))
         (envs3 (scan-environments cleaned-lines))
         ;; 修复每行括号
         (fixed-lines (fix-env-parens cleaned-lines envs3))
         ;; 收尾补回仍未闭合但已经可安全定位的 env（例如被中间阶段误删的顶层右标记）
         (completed-lines (insert-remaining-right-tags fixed-lines lines-with-right-tags))
         (best-lines (or (first-healthy-lines
                           (list completed-lines
                                 fixed-lines
                                 cleaned-lines
                                 lines-with-right-tags)
                           ) ;list
                         normalized-lines))
         (canonical-lines (canonicalize-right-tag-lines best-lines))
         ) ;best-lines
        (if (content-healthy? canonical-lines)
          canonical-lines
          best-lines
        ) ;if
      ) ;let*
    ) ;define

    ;; 运行完整修复流程：
    ;; - 即使原内容已经平衡，也照常规范化显式右标记
    ;; - 每轮都会基于最新内容继续修复，直到结果稳定或达到轮次上限
    (define (fix-content content)
      (let loop ((current-content content)
                 (remaining-rounds 4))
        (let* ((lines (string->lines current-content))
               (fixed-lines (run-goldfix-lines lines))
               (fixed-content (lines->string fixed-lines)))
          (if (or (= remaining-rounds 1)
                  (string=? fixed-content current-content))
            fixed-content
            (loop fixed-content (- remaining-rounds 1))
          ) ;if
        ) ;let*
      ) ;let
    ) ;define

    (define (run-goldfix file-path)
      (display (fix-content (read-file-exact-content file-path)))
    ) ;define

    ;; 原地更新文件
    (define (update-file-in-place file-path content)
      (with-output-to-file file-path
        (lambda ()
          (display content)
        ) ;lambda
      ) ;with-output-to-file
    ) ;define

    ;; 运行goldfix并返回字符串（用于原地更新）
    (define (run-goldfix-to-string file-path)
      (fix-content (read-file-exact-content file-path))
    ) ;define

    ;; 主入口函数
    (define (main)
      (let ((args (cddr (argv))))
        (cond
          ;; 没有参数
          ((null? args)
           (display "错误: 缺少文件参数")
           (newline)
           (display "用法: goldfix [-i|--in-place] <file.scm>")
           (newline)
           (display "  -i, --in-place  原地更新文件（默认输出到stdout）")
           (newline)
          ) ;
          
          ;; 有 -i 或 --in-place 参数
          ((or (string=? (car args) "-i")
               (string=? (car args) "--in-place"))
           (if (null? (cdr args))
             (begin
               (display "错误: 缺少文件参数")
               (newline)
             ) ;begin
             (let* ((file-path (cadr args))
                    (content (run-goldfix-to-string file-path)))
               (update-file-in-place file-path content)
               (display "文件已更新: ")
               (display file-path)
               (newline)
             ) ;let*
           ) ;if
          ) ;
          
          ;; 默认：输出到stdout
          (else
           (run-goldfix (car args))
          ) ;else
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
