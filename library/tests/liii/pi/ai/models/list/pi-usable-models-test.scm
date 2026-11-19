;; pi-usable-models
;; 已激活模型列表（白名单激活）：
;; 从 models-store 中提取各 provider 的可用模型条目，并归一化为统一格式。
;;
;; 语法
;; ----
;; (pi-usable-models models-store) -> list
;;
;; 参数
;; ----
;; models-store : alist
;; 用户模型目录或模型存储关联表。
;;
;; 返回值
;; ----
;; list
;; 条目为 (provider id name reasoning thinking-levels) alist，
;; 按 provider、id 字典序排序。

(import (scheme base)
  (srfi srfi-1)
  (liii check)
  (liii list)
  (liii sort)
  (liii string)
  (liii pi ai models list)
  (liii pi ai thinking)
) ;import

(check-set-mode! 'report-failed)

;; 夹具：包含 google 与 kimi-coding（白名单激活），google 下两个模型，kimi-coding 下一个模型
(define fixture-store
  '(("google"
     ("name" . "Google")
     ("models" . ((("id" . "gemini-2.5-flash")
                   ("name" . "Gemini 2.5 Flash")
                   ("reasoning" . #t))
                  (("id" . "gemini-2.5-pro")
                   ("name" . "Gemini 2.5 Pro")
                   ("reasoning" . #t)))))
    ("kimi-coding"
     ("name" . "Kimi Coding")
     ("models" . ((("id" . "k3")
                   ("name" . "Kimi K3")
                   ("reasoning" . #f))))))
) ;define

(let* ((models (pi-usable-models fixture-store))
       ;; 便于断言的派生视图
       (providers
         (delete-duplicates
           (map (lambda (m) (cdr (assoc 'provider m))) models)
         ) ;delete-duplicates
       ) ;providers
       (keys
         (map
           (lambda (m) (string-append (cdr (assoc 'provider m)) "/" (cdr (assoc 'id m))))
           models
         ) ;map
       ) ;keys
       (gemini-pro
         (find
           (lambda (m) (equal? (cdr (assoc 'id m)) "gemini-2.5-pro"))
           models
         ) ;find
       ) ;gemini-pro
       (kimi-entry
         (find
           (lambda (m) (equal? (cdr (assoc 'provider m)) "kimi-coding"))
           models
         ) ;find
       ) ;kimi-entry
      ) ;

  ;; 1. 已激活模型的 provider 全部返回：无凭证的 kimi-coding 同样返回
  (check-true (pair? models))
  (check providers => '("google" "kimi-coding"))

  ;; 2. 激活门控：白名单外 provider 不在 store 中则不返回
  (check-false (member "deepseek" providers))

  ;; 3. 模型白名单：google 只剩白名单内两个模型，白名单外模型不返回
  (check (filter (lambda (k) (string-starts? k "google/")) keys)
    =>
    '("google/gemini-2.5-flash" "google/gemini-2.5-pro")
  ) ;check

  ;; 4. 条目字段完整：provider/id/name/reasoning/thinking-levels 齐备
  (check-true (pair? gemini-pro))
  (check (cdr (assoc 'provider gemini-pro)) => "google")
  (check (cdr (assoc 'name gemini-pro)) => "Gemini 2.5 Pro")
  (check (cdr (assoc 'reasoning gemini-pro)) => #t)
  (check (cdr (assoc 'thinking-levels gemini-pro))
    =>
    (pi-model-thinking-levels gemini-pro)
  ) ;check

  ;; 5. 排序：provider 字典序，同 provider 内 id 字典序
  (check (length keys) => (length (delete-duplicates keys)))
  (check keys => (list-sort string<? keys))
) ;let*

(check-report)
