;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi tui common)
  (export %keyword-value detect-git-branch)
  (import (scheme base) (liii either) (liii string) (liii subprocess))
  (begin

    ;; 检测当前工作区所在的 git 分支名称，探测失败时返回 #f
    (define (detect-git-branch)
      (catch #t
        (lambda ()
          (let ((res (run-either '("git" "branch" "--show-current")
                       :stdout  'capture
                       :stderr  'discard
                       :timeout 2
                     ) ;run-either
                ) ;res
               ) ;
            (if (either-right? res)
              (let ((b (string-trim-both (to-right res))))
                (if (string-null? b) #f b)
              ) ;let
              #f
            ) ;if
          ) ;let
        ) ;lambda
        (lambda args #f)
      ) ;catch
    ) ;define

    ;; 从关键字参数列表中取出 key 对应的值，取不到时返回 default
    (define (%keyword-value key opts default)
      (let loop
        ((rest opts))
        (cond ((null? rest) default)
              ((and (pair? (cdr rest)) (eq? (car rest) key)) (cadr rest))
              ((null? (cdr rest)) default)
              (else (loop (cddr rest)))
        ) ;cond
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
