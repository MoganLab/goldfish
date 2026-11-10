;;
;; Ported from @earendil-works/pi-coding-agent (https://github.com/earendil-works/pi)
;; Original work Copyright (c) 2025 Mario Ortiz Manero / Earendil Works
;; Goldfish Scheme port Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 思考级别（thinking level）纯函数层：
;; - pi-thinking-levels：级别全集（对齐上游 EXTENDED_THINKING_LEVELS）
;; - pi-default-thinking-level：默认级别（对齐上游 DEFAULT_THINKING_LEVEL）
;; - pi-model-thinking-levels：按模型能力得出可用级别集
;;   （对齐上游 getSupportedThinkingLevels：reasoning 为假只支持 off；
;;    thinkingLevelMap 中映射为 null 的级别不支持；
;;    xhigh/max 需要目录里给出显式映射）
;; - pi-clamp-thinking-level：把请求级别收敛到最近的支持级别
;;   （对齐上游 clampThinkingLevel：先向高找，再向低找）
;; - pi-resolve-thinking-level：按「CLI > per-model > 全局 > 默认」解析生效级别
;;   （对齐上游 getDefaultThinkingLevel / getModelThinkingLevel 的优先级）
(define-library (liii pi ai thinking)
  (export pi-thinking-levels pi-default-thinking-level pi-thinking-level?
    pi-json-true? pi-model-supports-reasoning? pi-model-thinking-levels
    pi-clamp-thinking-level pi-resolve-thinking-level
  ) ;export
  (import (scheme base) (liii list) (liii string))
  (begin

    ;; 级别全集：off → max，由弱到强
    (define pi-thinking-levels
      '("off" "minimal" "low" "medium" "high" "xhigh" "max")
    ) ;define

    ;; 默认级别：用户未显式指定时的兜底值
    (define pi-default-thinking-level "medium")

    ;; 级别是否合法（级别全集内）
    (define (pi-thinking-level? level)
      (and (string? level) (member level pi-thinking-levels) #t)
    ) ;define

    ;; 键归一为字符串（字符串键与符号键等价）
    (define (%key->string k)
      (cond ((string? k) k)
            ((symbol? k) (symbol->string k))
            (else #f)
      ) ;cond
    ) ;define

    ;; alist 取值：键可为字符串或符号；缺键返回 #f
    ;; 注意：本实现的 assoc 对字符串键不按 equal? 比较，故自行遍历
    (define (%entry-ref entry key)
      (if (not (pair? entry))
        #f
        (let ((wanted (%key->string key)))
          (let loop
            ((rest entry))
            (cond ((not (pair? rest)) #f)
                  ((not (pair? (car rest))) (loop (cdr rest)))
                  ((equal? (%key->string (car (car rest))) wanted) (cdr (car rest)))
                  (else (loop (cdr rest)))
            ) ;cond
          ) ;let
        ) ;let
      ) ;if
    ) ;define

    ;; 非空字符串判定（空串与空白串都视为未设置）
    (define (%non-empty-string? v)
      (and (string? v) (not (string-null? (string-trim-both v))))
    ) ;define

    ;; JSON 布尔归一：string->json 解析出的是符号 true/false（不是 #t/#f），
    ;; 而内置目录里直接写的是 #t/#f。两者都要正确判定，
    ;; 否则 reasoning: false 会被当成真值。
    (define (pi-json-true? v)
      (cond ((eq? v #t) #t)
            ((eq? v 'true) #t)
            (else #f)
      ) ;cond
    ) ;define

    ;; 模型是否支持 reasoning：
    ;; - 模型条目缺失（元数据不可得）时按「支持」处理，返回全集级别
    ;;   （沿用 tools/pi 原有语义：无模型信息时不限制级别）
    ;; - 条目存在但无 reasoning 字段时按「不支持」处理
    ;; - JSON 布尔可能是符号 true/false，必须归一化后再判真假
    (define (pi-model-supports-reasoning? model)
      (if (not (pair? model))
        #t
        (let ((reasoning (%entry-ref model "reasoning")))
          (cond ((eq? reasoning 'null) #f)
                (else (pi-json-true? reasoning))
          ) ;cond
        ) ;let
      ) ;if
    ) ;define

    ;; 模型可用级别集：入参为模型目录条目 alist
    ;; - reasoning 为假（模型不支持思考）时只支持 off
    ;; - thinkingLevelMap 中映射为 null（JSON null）的级别不支持
    ;; - xhigh/max 需要 thinkingLevelMap 里有显式映射才支持
    (define (pi-model-thinking-levels model)
      (if (pi-model-supports-reasoning? model)
        (let ((level-map (%entry-ref model "thinkingLevelMap")))
          (filter
            (lambda (level)
              (let ((mapped (%entry-ref level-map level)))
                (cond ((eq? mapped 'null) #f)
                      ((or (string=? level "xhigh") (string=? level "max")) (not (eq? mapped #f)))
                      (else #t)
                ) ;cond
              ) ;let
            ) ;lambda
            pi-thinking-levels
          ) ;filter
        ) ;let
        '("off")
      ) ;if
    ) ;define

    ;; 从 idx 起向更低档扫描可用级别（找不到时取可用集首项）
    (define (%scan-thinking-level-down idx levels)
      (cond ((< idx 0) (car levels))
            ((member (list-ref pi-thinking-levels idx) levels)
             (list-ref pi-thinking-levels idx)
            ) ;
            (else (%scan-thinking-level-down (- idx 1) levels))
      ) ;cond
    ) ;define

    ;; 从 idx 起向更高档扫描可用级别；更高档没有时向更低档扫描
    (define (%scan-thinking-level-up idx levels)
      (let ((total (length pi-thinking-levels)))
        (cond ((>= idx total) (%scan-thinking-level-down (- idx 1) levels))
              ((member (list-ref pi-thinking-levels idx) levels)
               (list-ref pi-thinking-levels idx)
              ) ;
              (else (%scan-thinking-level-up (+ idx 1) levels))
        ) ;cond
      ) ;let
    ) ;define

    ;; 把请求级别收敛到最近的支持级别：先向更高档找，再向更低档找；
    ;; 级别不在全集内时取可用集首项；可用集为空时回退 off
    (define (pi-clamp-thinking-level level available-levels)
      (let ((levels (if (pair? available-levels) available-levels '("off"))))
        (cond ((member level levels) level)
              ((not (pi-thinking-level? level)) (car levels))
              (else
                (%scan-thinking-level-up (list-index (lambda (item) (equal? item level)) pi-thinking-levels)
                  levels
                ) ;%scan-thinking-level-up
              ) ;else
        ) ;cond
      ) ;let
    ) ;define

    ;; 解析生效级别：CLI 显式值 > per-model 覆盖 > 全局默认 > 默认级别（medium），
    ;; 最后按模型可用级别收敛
    (define (pi-resolve-thinking-level cli-level per-model-level global-level
              available-levels
            ) ;pi-resolve-thinking-level
      (let ((requested (cond ((%non-empty-string? cli-level) cli-level)
                             ((%non-empty-string? per-model-level) per-model-level)
                             ((%non-empty-string? global-level) global-level)
                             (else pi-default-thinking-level)
                       ) ;cond
            ) ;requested
           ) ;
        (pi-clamp-thinking-level requested available-levels)
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
