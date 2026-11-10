;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; Anthropic prompt caching 断点构造。
;;
;; 对齐上游 pi 的实现，cache_control: {"type": "ephemeral"} 断点挂在三处：
;; 1. system 提示词块（anthropic-system-blocks）
;; 2. 最后一个工具声明（anthropic-tool-decls-add-cache-control）
;; 3. 最后一条 user 消息的最后一个 content 块（anthropic-messages-add-cache-control）
;; 断点之前的内容（tools + system + 历史 messages）跨轮字节级一致时，
;; 供应商可直接复用缓存前缀。
(define-library (liii pi ai api anthropic-messages cache)
  (import (scheme base) (liii list))
  (export anthropic-system-blocks
    anthropic-tool-decls-add-cache-control
    anthropic-messages-add-cache-control
  ) ;export
  (begin

    (define %cache-control '(("type" . "ephemeral")))

    (define (%assoc-val key obj default-val)
      (let ((p (assoc key obj)))
        (if (pair? p) (cdr p) default-val)
      ) ;let
    ) ;define

    (define (%has-cache-control? blk)
      (and (pair? (assoc "cache_control" blk)) #t)
    ) ;define

    (define (%blk-add-cache-control blk)
      (if (%has-cache-control? blk)
        blk
        (append blk (list (cons "cache_control" %cache-control)))
      ) ;if
    ) ;define

    (define (%string-content->blocks content)
      (list->vector (list (list (cons "type" "text")
                            (cons "text" content)
                            (cons "cache_control" %cache-control)
                          ) ;list
                    ) ;list
      ) ;list->vector
    ) ;define

    ;; system 提示词转为挂断点的 content block 数组
    (define (anthropic-system-blocks system-prompt)
      (%string-content->blocks system-prompt)
    ) ;define

    ;; 仅最后一个工具声明挂断点，其余保持原样
    (define (anthropic-tool-decls-add-cache-control tool-decls)
      (if (null? tool-decls)
        '()
        (let ((rev (reverse tool-decls)))
          (reverse (cons (%blk-add-cache-control (car rev)) (cdr rev)))
        ) ;let
      ) ;if
    ) ;define

    ;; 替换消息 alist 中的 content 键
    (define (%msg-set-content msg new-content)
      (map
        (lambda (kv) (if (equal? (car kv) "content") (cons "content" new-content) kv))
        msg
      ) ;map
    ) ;define

    (define (%vector-map-last vec fn)
      (if (= (vector-length vec) 0)
        vec
        (let ((len (vector-length vec)) (out (make-vector (vector-length vec))))
          (let loop
            ((i 0))
            (if (= i len)
              out
              (begin
                (vector-set! out
                  i
                  (if (= i (- len 1)) (fn (vector-ref vec i)) (vector-ref vec i))
                ) ;vector-set!
                (loop (+ i 1))
              ) ;begin
            ) ;if
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    ;; 最后一条消息若是 user，给末尾 content 块挂断点：
    ;; content 为字符串时转为带断点的 text 块数组
    (define (anthropic-messages-add-cache-control messages)
      (if (or (not (vector? messages)) (= (vector-length messages) 0))
        messages
        (let* ((last-idx (- (vector-length messages) 1))
               (last-msg (vector-ref messages last-idx))
               (role (%assoc-val "role" last-msg ""))
               (content (%assoc-val "content" last-msg #f))
               (new-msg
                 (if (not (equal? role "user"))
                   last-msg
                   (cond ((string? content)
                          (%msg-set-content last-msg (%string-content->blocks content))
                         ) ;
                         ((vector? content)
                          (%msg-set-content last-msg (%vector-map-last content %blk-add-cache-control))
                         ) ;
                         (else last-msg)
                   ) ;cond
                 ) ;if
               ) ;new-msg
              ) ;
          (let ((out (make-vector (vector-length messages))))
            (let loop
              ((i 0))
              (if (= i (vector-length messages))
                out
                (begin
                  (vector-set! out i (if (= i last-idx) new-msg (vector-ref messages i)))
                  (loop (+ i 1))
                ) ;begin
              ) ;if
            ) ;let
          ) ;let
        ) ;let*
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
