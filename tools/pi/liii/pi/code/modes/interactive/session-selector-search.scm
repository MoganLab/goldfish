;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 会话选择器的搜索过滤与分层打分：
;; 1. 对会话元数据与消息正文分开缓存：元数据（name/id/cwd/first-message）零 I/O 立即可搜，
;;    正文（all-messages-text）使用字节级 ASCII 快速小写化，并支持增量分片预热；
;; 2. 分层打分（对齐上游 session-selector-search.ts）：
;;    Tier 1 (0): 会话名子串命中；
;;    Tier 2 (1000000): id / 首条消息 / 消息正文 / cwd 子串命中（正文不做模糊散布匹配）；
;;    Tier 3 (2000000+): 仅对 name 与 cwd 做模糊匹配；
;; 3. 单个会话读取/规范化异常时降级为不匹配，不拖垮整个选择器。
(define-library (liii pi code modes interactive session-selector-search)
  (import (scheme base)
    (scheme char)
    (scheme time)
    (liii string)
    (liii list)
    (liii sort)
    (liii unicode)
    (liii hash-table)
    (liii comparator)
    (liii tui)
    (liii pi code core session-list)
  ) ;import
  (export make-session-search-cache
    session-selector-filter-sessions
    session-selector-warm-batch!
  ) ;export
  (begin

    (define (make-session-search-cache)
      (make-hash-table (make-eq-comparator))
    ) ;define

    (define (%normalize-text str)
      (if (string? str) (%fast-downcase (utf8-replace-invalid str)) "")
    ) ;define

    ;; 字节级 ASCII 快速小写化：UTF-8 多字节序列不会包含 0x41..0x5A，
    ;; 因此对合法 UTF-8 文本安全，避免 utf8-string-map 构造百万级字符链表。
    (define (%fast-downcase str)
      (let* ((bv (string->byte-vector str)) (len (bytevector-length bv)))
        (let loop
          ((i 0) (changed? #f))
          (if (>= i len)
            (if changed? (byte-vector->string bv) str)
            (let ((b (bytevector-u8-ref bv i)))
              (if (and (>= b 65) (<= b 90))
                (begin
                  (bytevector-u8-set! bv i (+ b 32))
                  (loop (+ i 1) #t)
                ) ;begin
                (loop (+ i 1) changed?)
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let*
    ) ;define

    (define (%normalize-body str)
      (if (string? str) (%fast-downcase (utf8-replace-invalid str)) "")
    ) ;define

    (define (%meta-entry cache session)
      (let ((hit (hash-table-ref/default cache session #f)))
        (or hit
          (let ((entry
                  (catch #t
                    (lambda ()
                      (vector (%normalize-text (pi-session-info-name session))
                        (%normalize-text (pi-session-info-id session))
                        (%normalize-text (pi-session-info-cwd session))
                        (%normalize-text (pi-session-info-first-message session))
                        #f
                      ) ;vector
                    ) ;lambda
                    (lambda _ 'bad)
                  ) ;catch
                ) ;entry
               ) ;
            (hash-table-set! cache session entry)
            entry
          ) ;let
        ) ;or
      ) ;let
    ) ;define

    (define (%ensure-body! entry session)
      (let ((body (vector-ref entry 4)))
        (if (string? body)
          body
          (let ((loaded
                  (catch #t
                    (lambda () (%normalize-body (pi-session-info-all-messages-text session)))
                    (lambda _ "")
                  ) ;catch
                ) ;loaded
               ) ;
            (vector-set! entry 4 loaded)
            loaded
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (%tokens-in-string? tokens text)
      (and (not (string-null? text))
        (every (lambda (tok) (string-contains? text tok)) tokens)
      ) ;and
    ) ;define

    (define (%tokens-in-any? tokens texts)
      (every
        (lambda (tok) (any (lambda (text) (string-contains? text tok)) texts))
        tokens
      ) ;every
    ) ;define

    (define (%fuzzy-token-score tok name cwd)
      (let ((r1 (and (not (string-null? name)) (fuzzy-match tok name)))
            (r2 (and (not (string-null? cwd)) (fuzzy-match tok cwd)))
           ) ;
        (let ((s1 (and r1 (cdr (assoc 'matches r1)) (cdr (assoc 'score r1))))
              (s2 (and r2 (cdr (assoc 'matches r2)) (cdr (assoc 'score r2))))
             ) ;
          (cond ((and s1 s2) (min s1 s2))
                (s1 s1)
                (s2 s2)
                (else #f)
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (%fuzzy-tokens-score tokens name cwd)
      (fold
        (lambda (tok total)
          (and total
            (let ((score (%fuzzy-token-score tok name cwd)))
              (and score (+ total score))
            ) ;let
          ) ;and
        ) ;lambda
        0
        tokens
      ) ;fold
    ) ;define

    (define (%score-session cache tokens session incremental?)
      (let ((entry (%meta-entry cache session)))
        (if (eq? entry 'bad)
          #f
          (let* ((name (vector-ref entry 0))
                 (id (vector-ref entry 1))
                 (cwd (vector-ref entry 2))
                 (first-msg (vector-ref entry 3))
                 (body (if incremental? (or (vector-ref entry 4) "") (%ensure-body! entry session))
                 ) ;body
                ) ;
            (cond ((%tokens-in-string? tokens name) 0)
                  ((%tokens-in-any? tokens (list name id cwd first-msg body)) 1000000)
                  (else
                    (let ((fscore (%fuzzy-tokens-score tokens name cwd)))
                      (and fscore (+ 2000000 fscore))
                    ) ;let
                  ) ;else
            ) ;cond
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    (define* (session-selector-filter-sessions cache
               sessions
               query
               sort-mode
               (incremental? #f)
             ) ;session-selector-filter-sessions
      (let* ((trimmed (string-trim-both (%normalize-text query)))
             (tokens (if (string-null? trimmed) '() (string-tokenize trimmed)))
            ) ;
        (if (null? tokens)
          sessions
          (let ((scored
                  (filter (lambda (pair) (car pair))
                    (map (lambda (s) (cons (%score-session cache tokens s incremental?) s))
                      sessions
                    ) ;map
                  ) ;filter
                ) ;scored
               ) ;
            (map cdr
              (if (eq? sort-mode 'relevance)
                (list-sort
                  (lambda (a b)
                    (if (= (car a) (car b))
                      (pi-session-info-newer? (cdr a) (cdr b))
                      (< (car a) (car b))
                    ) ;if
                  ) ;lambda
                  scored
                ) ;list-sort
                scored
              ) ;if
            ) ;map
          ) ;let
        ) ;if
      ) ;let*
    ) ;define*

    ;; 按时间片预算预热未索引会话的 all-messages-text，返回 (warmed . pending?)
    (define* (session-selector-warm-batch! cache sessions (budget-sec 0.025))
      (let ((deadline (+ (current-second) budget-sec)))
        (let loop
          ((rest sessions) (warmed 0))
          (cond ((null? rest) (cons warmed #f))
                ((and (> warmed 0) (>= (current-second) deadline)) (cons warmed #t))
                (else
                  (let* ((s (car rest))
                         (entry (%meta-entry cache s))
                         (need?
                           (and (vector? entry) (not (string? (vector-ref entry 4))))
                         ) ;need?
                        ) ;
                    (if need?
                      (begin
                        (%ensure-body! entry s)
                        (loop (cdr rest) (+ warmed 1))
                      ) ;begin
                      (loop (cdr rest) warmed)
                    ) ;if
                  ) ;let*
                ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define*

  ) ;begin
) ;define-library
