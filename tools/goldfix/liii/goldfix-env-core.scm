;;; Goldfix Env Core 模块
;;; Environment 记录类型与基础树操作
;;;
;;; Copyright (c) 2026 Liii Network
;;; All Rights Reserved

(define-library (liii goldfix-env-core)
  (import (scheme base))
  (import (liii goldfix-comment))
  (import (liii goldfix-lint))

  (export make-env)
  (export env?)
  (export env-tag)
  (export env-lparen-line)
  (export env-lparen-col)
  (export env-parent)
  (export env-children)
  (export env-set-children!)
  (export env-rparen-line)
  (export env-set-rparen-line!)

  (export make-env-detail)
  (export env-detail?)
  (export env-detail-env)
  (export env-detail-close-line)
  (export env-detail-explicit-rparen-line)
  (export env-detail-set-close-line!)
  (export env-detail-set-explicit-rparen-line!)

  (export make-paren-node)
  (export paren-node?)
  (export paren-node-line)
  (export paren-node-col)
  (export paren-node-env)
  (export paren-node-detail)
  (export paren-node-tag)

  (export find-open-parent-by-col)
  (export add-child!)
  (export unwind-stack-before-col)

  (begin
    (define-record-type env
      (make-raw-env tag lparen-line lparen-col parent)
      env?
      (tag env-tag)
      (lparen-line env-lparen-line)
      (lparen-col env-lparen-col)
      (parent env-parent)
      (children env-children env-set-children!)
      (rparen-line env-rparen-line env-set-rparen-line!)
    ) ;define-record-type

    (define* (make-env tag
                       (lparen-line (error "make-env requires :lparen-line"))
                       (lparen-col (error "make-env requires :lparen-col"))
                       (parent #f))
      (let ((env (make-raw-env tag lparen-line lparen-col parent)))
        (env-set-children! env '())
        env
      ) ;let
    ) ;define*

    (define-record-type env-detail
      (make-env-detail env close-line explicit-rparen-line)
      env-detail?
      (env env-detail-env)
      (close-line env-detail-close-line env-detail-set-close-line!)
      (explicit-rparen-line env-detail-explicit-rparen-line env-detail-set-explicit-rparen-line!)
    ) ;define-record-type

    (define-record-type paren-node
      (make-raw-paren-node line col env detail tag)
      paren-node?
      (line paren-node-line)
      (col paren-node-col)
      (env paren-node-env)
      (detail paren-node-detail)
      (tag paren-node-tag)
    ) ;define-record-type

    (define (make-paren-node line col env detail . args)
      (make-raw-paren-node line
                           col
                           env
                           detail
                           (if (null? args) "" (car args))
      ) ;make-raw-paren-node
    ) ;define

    ;; 对每行首个 form，优先按缩进找父环境。
    ;; 这样当上一段代码缺少右括号时，像 (else 这类回到更浅缩进的 sibling
    ;; 不会被错误地吸附到还未闭合的内层环境里。
    (define (find-open-parent-by-col stack col)
      (let loop ((rest stack))
        (cond
          ((null? rest) #f)
          ((and (paren-node-env (car rest))
                (< (env-lparen-col (paren-node-env (car rest))) col)
           ) ;and
           (paren-node-env (car rest))
          ) ;
          (else
           (loop (cdr rest))
          ) ;else
        ) ;cond
      ) ;let
    ) ;define

    (define (add-child! parent child)
      (when parent
        (env-set-children! parent (cons child (env-children parent)))
      ) ;when
    ) ;define

    (define (previous-significant-line-number lines line-num)
      (let loop ((current (- line-num 1)))
        (if (< current 1)
          0
          (if (blank-or-comment-line? (list-ref lines (- current 1)))
            (loop (- current 1))
            current
          ) ;if
        ) ;if
      ) ;let
    ) ;define

    (define (line-net-open-count line)
      (let-values (((paren-counts _block-depth _in-string _escape-next)
                    (count-parens-with-state line 0 #f #f)))
        (- (car paren-counts) (cdr paren-counts))
      ) ;let-values
    ) ;define

    ;; 当新行的第一个 form 回到更浅或同级缩进时，
    ;; 将更深层的未闭合环境视作在上一有效行结束。
    (define (unwind-stack-before-col lines stack col line-num)
      (let ((close-line (previous-significant-line-number lines line-num)))
        (define (mark-closed! detail)
          (when (and (> close-line 0)
                     (not (env-detail-close-line detail)))
            (env-detail-set-close-line! detail close-line)
          ) ;when
        ) ;define
        (let loop ((rest stack) (pending-raw '()))
          (cond
            ((null? rest)
             '()
            ) ;
            (else
             (let* ((node (car rest))
                    (node-col (paren-node-col node))
                    (detail (paren-node-detail node)))
               (cond
                 ;; 普通括号节点先暂存。
                 ;; 如果下方最近的 env 仍然保留，它们也应保留；
                 ;; 如果下方 env 因缩进变化被提前关闭，这些裸括号也应一并丢弃，
                 ;; 否则后续真实的 ) 会错误地继续向外层 env 泄漏。
                 ((not detail)
                  (loop (cdr rest) (cons node pending-raw))
                 ) ;
                 ;; 如果上一有效行就是当前 env 的起始行，且该行净 open 数仍为正，
                 ;; 说明这个 env 在本行还没真正闭合，不能因为出现同级 sibling
                 ;; 就把它提前判定为“单行 env”。
                 ((and (= node-col col)
                       (paren-node-env node)
                       (= (env-lparen-line (paren-node-env node)) close-line)
                       (> (line-net-open-count (list-ref lines (- close-line 1))) 0)
                  ) ;and
                  (append (reverse pending-raw) rest)
                 ) ;
                 ((< node-col col)
                  (append (reverse pending-raw) rest)
                 ) ;
                 (else
                  (mark-closed! detail)
                  (loop (cdr rest) '())
                 ) ;else
               ) ;cond
             ) ;let*
            ) ;else
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
