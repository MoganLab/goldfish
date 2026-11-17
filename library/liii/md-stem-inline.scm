
(define-library (liii md-stem-inline)
  (import (scheme base))
  (import (liii cmark node))
  (import (only (liii cmark parser) parse-inlines))
  (import (liii cmark utils common))
  (export inline->stem-list)
  (export nodes->stem-list)
  (export node->stem-list)
  (export merge-adjacent-strings)
  (export node-data-ref)
  (begin
    (define (inline->stem-list str . maybe-refmap)
      (let ((refmap (if (null? maybe-refmap) #f (car maybe-refmap))))
        (merge-adjacent-strings (nodes->stem-list (parse-inlines str refmap)))
      ) ;let
    ) ;define

    (define (nodes->stem-list nodes)
      (if (null? nodes)
        '()
        (append (node->stem-list (car nodes)) (nodes->stem-list (cdr nodes)))
      ) ;if
    ) ;define

    ;; image alt 渲染为纯文本（CommonMark §6.6：嵌套 link/em/image 取文本）
    (define (node->plain-text node)
      (let ((type (cmark-node-type node)))
        (cond ((eq? type 'text) (node-data-ref node 'content ""))
              ((eq? type 'code) (node-data-ref node 'literal ""))
              ((eq? type 'html_inline) (node-data-ref node 'literal ""))
              ((or (eq? type 'softbreak) (eq? type 'linebreak)) " ")
              ((or (eq? type 'emph) (eq? type 'strong) (eq? type 'strikeout))
               (nodes->plain-text (cmark-node-children node))
              ) ;
              ((or (eq? type 'link) (eq? type 'image))
               (nodes->plain-text (cmark-node-children node))
              ) ;
              (else "")
        ) ;cond
      ) ;let
    ) ;define

    (define (nodes->plain-text nodes)
      (apply string-append (map node->plain-text nodes))
    ) ;define

    ;; 单个子节点 → alt 片段：math/emoji 保留语义结构（§3.11）
    (define (math-inline-stem node)
      (list (list 'math-inline (node-data-ref node 'literal "")))
    ) ;define

    (define (emoji-stem node)
      (list (list 'emoji (node-data-ref node 'shortcode "")))
    ) ;define

    ;; 收集 image alt 的子节点片段（§3.11）：math/emoji 保留语义结构，
    ;; 其余节点取纯文本；空 alt 由调用方输出 ""
    (define (collect-image-alt-items nodes)
      (let collect
        ((cs nodes) (acc '()))
        (if (null? cs)
          (reverse acc)
          (let ((t (cmark-node-type (car cs))))
            (collect (cdr cs)
              (append
                (cond ((eq? t 'math_inline) (math-inline-stem (car cs)))
                      ((eq? t 'emoji) (emoji-stem (car cs)))
                      (else (list (node->plain-text (car cs))))
                ) ;cond
                acc
              ) ;append
            ) ;collect
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (node->stem-list node)
      (case (cmark-node-type node)
            ((text) (list (node-data-ref node 'content "")))
            ((code) (list (list 'code (node-data-ref node 'literal ""))))
            ((emoji) (list (list 'emoji (node-data-ref node 'shortcode ""))))
            ((html_inline)
             (list (list 'raw-inline "html" (node-data-ref node 'literal "")))
            ) ;
            ((math_inline) (list (list 'math-inline (node-data-ref node 'literal ""))))
            ((softbreak) '((soft-break)))
            ((linebreak) '((line-break)))
            ((emph) (list (cons 'em (nodes->stem-list (cmark-node-children node)))))
            ((strong) (list (cons 'strong (nodes->stem-list (cmark-node-children node)))))
            ((strikeout)
             (list (cons 'strikeout (nodes->stem-list (cmark-node-children node))))
            ) ;
            ((link)
             (let ((url (node-data-ref node 'url "")) (title (node-data-ref node 'title "")))
               (list
                 (cons 'link
                   (cons url (cons title (nodes->stem-list (cmark-node-children node))))
                 ) ;cons
               ) ;list
             ) ;let
            ) ;
            ((image)
             ;; alt：CommonMark §6.6 中 emph/strong/link/image 取文本，
             ;; math/emoji（项目扩展节点）保留语义结构；空 alt 输出 ""
             (let* ((url (node-data-ref node 'url ""))
                    (title (node-data-ref node 'title ""))
                    (children (cmark-node-children node))
                    (alt-items (merge-adjacent-strings (collect-image-alt-items children)))
                   ) ;
               (list
                 (cons 'image (cons url (cons title (if (null? alt-items) '("") alt-items))))
               ) ;list
             ) ;let*
            ) ;
            (else '())
      ) ;case
    ) ;define

    (define (merge-adjacent-strings items)
      (let loop
        ((remaining items) (result '()))
        (cond ((null? remaining) (reverse result))
              ((and (pair? result) (string? (car remaining)) (string? (car result)))
               (loop (cdr remaining)
                 (cons (string-append (car result) (car remaining)) (cdr result))
               ) ;loop
              ) ;
              (else (loop (cdr remaining) (cons (car remaining) result)))
        ) ;cond
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
