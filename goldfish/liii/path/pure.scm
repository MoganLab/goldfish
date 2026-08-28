    ;; ; 解析末段的点分隔结构,返回 (values stem-list suffix-list)。
    ;; ; 隐藏文件(.bashrc)、无点、"."/".." 整体作 stem 无后缀。
    ;; ; 末尾点(foo.)当作空后缀:stem="foo", suffix=".", 对齐 Python 3.14+ pathlib。
    (define (split-name-dots name)
      (cond ((or (string=? name ".") (string=? name "..")) (cons (list name) '()))
            (else (let ((splits (string-split name #\.)))
                    (if (or (<= (length splits) 1) (string=? (car splits) ""))
                      (cons (list name) '())
                      (let* ((rev (reverse splits)) (suffix-seg (car rev)) (stem-segs (reverse (cdr rev))))
                        (cons stem-segs (list (string-append "." suffix-seg)))
                      ) ;let*
                    ) ;if
                  ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; ; 过滤 path-from-parts 中首段 anchor 之后的空段/分隔符 stub。
    ;; ; 同时识别 posix "/" 与 Windows "\\" 作为分隔符(避免污染)。
    (define (clean-tail parts)
      (vector-filter (lambda (part)
                       (not (or (string-null? part) (string=? part "/") (string=? part "\\")))
                     ) ;lambda
        (vector-drop parts 1)
      ) ;vector-filter
    ) ;define

    ;; ; 字符类 [..] 匹配单字符 ch。pattern[j] 是 '['。
    ;; ; 返回 (values matched? next-index-after-])。
    (define (charclass-match-one pattern plen j ch)
      (let ((negate (and (< (+ j 1) plen) (char=? (string-ref pattern (+ j 1)) #\^))))
        (let scan
          ((k (if negate (+ j 2) (+ j 1))) (matched #f))
          (cond ((or (>= k plen) (char=? (string-ref pattern k) #\]))
                 (cons (if negate (not matched) matched) (if (< k plen) (+ k 1) k))
                ) ;
                ((and (< (+ k 2) plen) (char=? (string-ref pattern (+ k 1)) #\-))
                 (let ((lo (string-ref pattern k)) (hi (string-ref pattern (+ k 2))))
                   (let ((hit (if (char<=? lo hi)
                                (and (char>=? ch lo) (char<=? ch hi))
                                (and (char>=? ch hi) (char<=? ch lo))
                              ) ;if
                         ) ;hit
                        ) ;
                     (scan (+ k 3) (or matched hit))
                   ) ;let
                 ) ;let
                ) ;
                (else (scan (+ k 1) (or matched (char=? (string-ref pattern k) ch))))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    ;; ; glob 单层匹配(支持 * / ? / [seq],不跨分隔符)。平台无关
    ;; ; (大小写敏感性由 path-match 在 Windows 类型下统一 downcase 处理)。
    (define (glob-match? pattern str)
      (let ((plen (string-length pattern)) (slen (string-length str)))
        (letrec ((match-at (lambda (p0 s0)
                             (cond ((= p0 plen) (= s0 slen))
                                   ((char=? (string-ref pattern p0) #\*)
                                    ;; * 尝试匹配 0..(slen-s0) 个字符
                                    (let try-star
                                      ((n 0))
                                      (cond ((match-at (+ p0 1) (+ s0 n)) #t)
                                            ((< (+ s0 n) slen) (try-star (+ n 1)))
                                            (else #f)
                                      ) ;cond
                                    ) ;let
                                   ) ;
                                   ((= s0 slen) #f)
                                   ((char=? (string-ref pattern p0) #\?) (match-at (+ p0 1) (+ s0 1)))
                                   ((char=? (string-ref pattern p0) #\[)
                                     (let* ((res (charclass-match-one pattern plen p0 (string-ref str s0)))
                                            (hit (car res))
                                            (next (cdr res))
                                           ) ;
                                       (and hit (match-at next (+ s0 1)))
                                     ) ;let*
                                    ) ;
                                   ((char=? (string-ref pattern p0) (string-ref str s0))
                                    (match-at (+ p0 1) (+ s0 1))
                                   ) ;
                                   (else #f)
                             ) ;cond
                           ) ;lambda
                 ) ;match-at
                ) ;
          (match-at 0 0)
        ) ;letrec
      ) ;let
    ) ;define
