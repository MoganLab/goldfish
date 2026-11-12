;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 页脚右下角模型标签组装：普通供应商 (provider) model • level；
;; all-in-one 伪模型精简为 all-in-one/<chain-id>，实际节点追加 -> 节点。
;; 纯函数层：只依赖入参，不依赖页脚状态记录，避免与 footer 循环依赖。
(define-library (liii pi code modes interactive footer-model)
  (export pi-footer-model-label pi-footer-model-part)
  (import (scheme base) (liii string))
  (begin

    ;; 组装 footer 右下角的模型显示串（对齐上游 pi）：
    ;; 模型支持 reasoning 时，off 也显式显示 " • thinking off"；
    ;; 不支持 reasoning 的模型只显示 model-id
    (define (pi-footer-model-label model-id thinking-level supports-thinking?)
      (if supports-thinking?
        (let ((tl (if (string? thinking-level) (string-trim-both thinking-level) "")))
          (if (or (string-null? tl) (string=? tl "off"))
            (string-append model-id " • thinking off")
            (string-append model-id " • " tl)
          ) ;if
        ) ;let
        model-id
      ) ;if
    ) ;define

    ;; 右侧模型串：all-in-one 精简为 all-in-one/<chain-id>（不显示伪模型括号与
    ;; 自身等级），普通供应商保持 (provider) model • level；有实际节点时追加
    ;; " -> (provider) model • level"，等级为收敛后的最终生效值
    (define (pi-footer-model-part provider model thinking-level
              supports-thinking? active-node
            ) ;pi-footer-model-part
      (let* ((aio? (equal? provider "all-in-one"))
             (base (if aio?
                     (string-append "all-in-one/" (or model ""))
                     (string-append (%provider-prefix provider)
                       (pi-footer-model-label model thinking-level supports-thinking?)
                     ) ;string-append
                   ) ;if
             ) ;base
             (node (%active-node-label active-node))
            ) ;
        (if node (string-append base " -> " node) base)
      ) ;let*
    ) ;define

    (define (%provider-prefix provider)
      (if (and provider (not (string-null? provider)))
        (string-append "(" provider ") ")
        ""
      ) ;if
    ) ;define

    ;; 当前节点串：(provider) model • level；第五项为实际上报的
    ;; supports-thinking?，缺省按支持处理以兼容旧三元组/四元组载荷
    (define (%active-node-label node)
      (and (pair? node)
        (let* ((p (car node))
               (m (cadr node))
               (tl (if (>= (length node) 3) (caddr node) "off"))
               (supports? (if (>= (length node) 5) (list-ref node 4) #t))
              ) ;
          (and (string? m)
            (not (string-null? m))
            (string-append (%provider-prefix (and (string? p) p))
              (pi-footer-model-label m tl supports?)
            ) ;string-append
          ) ;and
        ) ;let*
      ) ;and
    ) ;define

  ) ;begin
) ;define-library
