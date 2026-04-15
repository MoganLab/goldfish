(import (liii check)
        (liii goldfmt-format)
        (liii goldfmt-record)
        (liii goldfmt-scan)
        (liii raw-string)
) ;import

(check-set-mode! 'report-failed)

(define (child node index)
  (vector-ref (env-children node) index)
) ;define

;; format-node 位置信息
;; 验证 formatter 返回的新 node 树是否正确记录布局位置。
;;
;; 语法
;; ----
;; (format-node node column)
;;
;; 返回值
;; ------
;; values
;; 1. text : string? 格式化后的文本
;; 2. positioned-node : env? 或 atom? 带有布局位置的新 node
;;
;; 位置信息
;; --------
;; indent : node 左侧起始列，env 对应左括号所在列，atom 对应文本起始列。
;; left-line : node 开始输出的行号，从 1 开始。
;; right-line : node 完成输出的行号，从 1 开始。
;;
;; 说明
;; ----
;; `format-node` 不修改原始 scan 结果。
;; 原始 node 的 indent 仍为 -1，left-line/right-line 仍为 0。

;; atom：从非 0 列开始输出时，返回的新 atom 记录该列。
(let ((node (scan 'answer)))
  (call-with-values
    (lambda () (format-node node 4))
    (lambda (text positioned)
      (check text => "answer")
      (check (atom-indent node) => -1)
      (check (atom-left-line node) => 0)
      (check (atom-right-line node) => 0)
      (check (atom-indent positioned) => 4)
      (check (atom-left-line positioned) => 1)
      (check (atom-right-line positioned) => 1)
    ) ;lambda
  ) ;call-with-values
) ;let

;; inline env：父子节点都在同一行时，仍然记录各自的起始列。
(let ((node (scan '(+ (f x) 1))))
  (call-with-values
    (lambda () (format-node node 0))
    (lambda (text positioned)
      (check text => "(+ (f x) 1)")
      (check (env-indent node) => -1)
      (check (env-indent positioned) => 0)
      (check (env-left-line positioned) => 1)
      (check (env-right-line positioned) => 1)
      (let ((call-node (child positioned 0))
            (one (child positioned 1)))
        (check (env-indent call-node) => 3)
        (check (env-left-line call-node) => 1)
        (check (env-right-line call-node) => 1)
        (check (atom-indent (child call-node 0)) => 6)
        (check (atom-left-line (child call-node 0)) => 1)
        (check (atom-right-line (child call-node 0)) => 1)
        (check (atom-indent one) => 9)
        (check (atom-left-line one) => 1)
      (check (atom-right-line one) => 1))
    ) ;lambda
  ) ;call-with-values
) ;let

;; 函数 define：签名环境在第一行，body 环境另起一行。
;; format-node 返回新的 positioned node，原始 scan 结果保持未布局状态。
(let ((node (scan '(define (f x) (+ x 1)))))
  (call-with-values
    (lambda () (format-node node 0))
    (lambda (text positioned)
        (check text
               => (&- #""
                       (define (f x)
                         (+ x 1)
                       ) ;define
                       ""
                  ) ;&-
        ) ;check
      (check (env-indent node) => -1)
      (check (env-left-line node) => 0)
      (check (env-right-line node) => 0)
      (check (env-indent (child node 0)) => -1)
      (check (env-indent positioned) => 0)
      (check (env-left-line positioned) => 1)
      (check (env-right-line positioned) => 3)
      (let ((signature (child positioned 0))
            (body (child positioned 1)))
        (check (env-indent signature) => 8)
        (check (env-left-line signature) => 1)
        (check (env-right-line signature) => 1)
        (check (env-indent body) => 2)
        (check (env-left-line body) => 2)
        (check (env-right-line body) => 2)
        (let ((x (child body 0))
              (one (child body 1)))
          (check (atom-indent x) => 5)
          (check (atom-left-line x) => 2)
          (check (atom-right-line x) => 2)
          (check (atom-indent one) => 7)
          (check (atom-left-line one) => 2)
          (check (atom-right-line one) => 2)
        ) ;let
      ) ;let
    ) ;lambda
  ) ;call-with-values
) ;let

;; let：bindings 环境在第一行，body 使用 parent+2 缩进。
(let
  ((node
     (scan '(let ((x (begin
                       (display "computing")
                       (compute-x arg1)))
                  (y 20))
              (+ x y))
     ) ;scan
   ) ;node
  ) ;
  (call-with-values
    (lambda () (format-node node 0))
    (lambda (text positioned)
      (check text
             => (&- #""
                     (let ((x (begin
                                (display "computing")
                                (compute-x arg1)
                              ) ;begin
                           ) ;x
                           (y 20)
                          ) ;
                       (+ x y)
                     ) ;let
                     ""
                ) ;&-
      ) ;check
      (check (env-indent node) => -1)
      (check (env-left-line node) => 0)
      (check (env-right-line node) => 0)
      (check (env-indent positioned) => 0)
      (check (env-left-line positioned) => 1)
      (check (env-right-line positioned) => 9)
      (let* ((bindings (child positioned 0))
             (binding-x (child bindings 0))
             (begin-node (child binding-x 0))
             (body (child positioned 1)))
        (check (env-indent bindings) => 5)
        (check (env-left-line bindings) => 1)
        (check (env-right-line bindings) => 7)
        (check (env-indent binding-x) => 6)
        (check (env-left-line binding-x) => 1)
        (check (env-right-line binding-x) => 5)
        (check (env-indent begin-node) => 9)
        (check (env-left-line begin-node) => 1)
        (check (env-right-line begin-node) => 4)
        (check (env-indent body) => 2)
        (check (env-left-line body) => 8)
        (check (env-right-line body) => 8)
      ) ;let*
    ) ;lambda
  ) ;call-with-values
) ;let

;; cond：第一个 clause 在第一行，后续 clause 与它对齐。
(let
  ((node
     (scan '(cond ((or (> x 0) (< x 0))
                   (begin
                     (display "non-zero")
                     (quote non-zero)))
                  (else (quote zero))))
   ) ;node
  ) ;
  (call-with-values
    (lambda () (format-node node 0))
    (lambda (text positioned)
      (check text
             => (&- #""
                     (cond ((or (> x 0) (< x 0))
                            (begin
                              (display "non-zero")
                              'non-zero
                            ) ;begin
                           ) ;
                           (else 'zero)
                     ) ;cond
                     ""
                ) ;&-
      ) ; check
      (check (env-indent node) => -1)
      (check (env-left-line node) => 0)
      (check (env-right-line node) => 0)
      (check (env-indent positioned) => 0)
      (check (env-left-line positioned) => 1)
      (check (env-right-line positioned) => 8)
      (let* ((clause1 (child positioned 0))
             (or-node (child clause1 0))
             (begin-node (child clause1 1))
             (clause2 (child positioned 1)))
        (check (env-indent clause1) => 6)
        (check (env-left-line clause1) => 1)
        (check (env-right-line clause1) => 6)
        (check (env-indent or-node) => 7)
        (check (env-left-line or-node) => 1)
        (check (env-right-line or-node) => 1)
        (check (env-indent begin-node) => 7)
        (check (env-left-line begin-node) => 2)
        (check (env-right-line begin-node) => 5)
        (check (env-indent clause2) => 6)
        (check (env-left-line clause2) => 7)
        (check (env-right-line clause2) => 7)
      ) ;let*
    ) ;lambda
  ) ;call-with-values
) ;let

(check-report)
