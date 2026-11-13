;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 会话选择器的搜索过滤与分层打分：
;; 1. 对会话文本做 UTF-8 消毒与小写化并按会话对象缓存，避免每次按键重复扫描大文本；
;; 2. 分层打分（对齐上游 session-selector-search.ts）：
;;    Tier 1 (0): 会话名子串命中；
;;    Tier 2 (1000000): id / 消息正文 / cwd 子串命中（正文不做模糊散布匹配）；
;;    Tier 3 (2000000+): 仅对 name 与 cwd 做模糊匹配；
;; 3. 单个会话读取/规范化异常时降级为不匹配，不拖垮整个选择器。
(define-library (liii pi code modes interactive session-selector-search)
  (import (scheme base)
    (scheme char)
    (liii string)
    (liii list)
    (liii sort)
    (liii unicode)
    (liii hash-table)
    (liii comparator)
    (liii tui)
    (liii pi code core session-list)
  ) ;import
  (export make-session-search-cache session-selector-filter-sessions)
  (begin

    (define (make-session-search-cache)
      (make-hash-table (make-eq-comparator))
    ) ;define

    (define (%normalize-text str)
      (if (string? str) (string-downcase (utf8-replace-invalid str)) "")
    ) ;define

    (define (%search-entry cache session)
      (let ((hit (hash-table-ref/default cache session #f)))
        (or hit
          (let ((entry
                  (catch #t
                    (lambda ()
                      (vector (%normalize-text (pi-session-info-name session))
                        (%normalize-text (pi-session-info-id session))
                        (%normalize-text (pi-session-info-cwd session))
                        (%normalize-text (pi-session-info-all-messages-text session))
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

    (define (%score-session cache tokens session)
      (let ((entry (%search-entry cache session)))
        (if (eq? entry 'bad)
          #f
          (let ((name (vector-ref entry 0))
                (id (vector-ref entry 1))
                (cwd (vector-ref entry 2))
                (all-text (vector-ref entry 3))
               ) ;
            (cond ((%tokens-in-string? tokens name) 0)
                  ((%tokens-in-any? tokens (list name id cwd all-text)) 1000000)
                  (else
                    (let ((fscore (%fuzzy-tokens-score tokens name cwd)))
                      (and fscore (+ 2000000 fscore))
                    ) ;let
                  ) ;else
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (session-selector-filter-sessions cache sessions query sort-mode)
      (let* ((trimmed (string-trim-both (%normalize-text query)))
             (tokens (if (string-null? trimmed) '() (string-tokenize trimmed)))
            ) ;
        (if (null? tokens)
          sessions
          (let ((scored
                  (filter (lambda (pair) (car pair))
                    (map (lambda (s) (cons (%score-session cache tokens s) s)) sessions)
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
    ) ;define

  ) ;begin
) ;define-library
