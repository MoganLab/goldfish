;; json->pi-tool-result
;; 将 (liii json) 解析出的 Scheme-JSON 对象转换为 <pi-tool-result> record。
;;
;; 语法
;; ----
;; (json->pi-tool-result obj)
;;
;; 参数
;; ----
;; obj : any?
;; Scheme-JSON alist 或已有的 <pi-tool-result> record。
;;
;; 返回值
;; ----
;; any?
;; <pi-tool-result> record 或 #f。

(import (liii check)
  (liii json)
  (liii pi ai tools result)
  (liii pi ai message record)
) ;import

(check-set-mode! 'report-failed)

(check-true (procedure? json->pi-tool-result))

;; 1. 幂等：已是 pi-tool-result 直接返回
(let ((tr (pi-tool-result "ok" #f)))
  (check (json->pi-tool-result tr) => tr)
) ;let

;; 2. JSON round-trip 与 isError 布尔归一
(let* ((tr1 (pi-tool-result "done" #f))
       (r1 (json->pi-tool-result (string->json (json->string (pi-tool-result->json tr1)))))
       (tr2 (pi-tool-result (list (pi-text-block "err")) #t))
       (r2 (json->pi-tool-result (string->json (json->string (pi-tool-result->json tr2)))))
      ) ;
  (check-true (pi-tool-result? r1))
  (check (pi-tool-result-is-error r1) => #f)
  (check-true (pi-text-block? (vector-ref (pi-tool-result-content r1) 0)))
  (check (pi-text-block-text (vector-ref (pi-tool-result-content r1) 0)) => "done")
  (check-true (pi-tool-result? r2))
  (check (pi-tool-result-is-error r2) => #t)
  (check (pi-text-block-text (vector-ref (pi-tool-result-content r2) 0)) => "err")
) ;let*

;; 3. 非 json-object / null / pi-tool-result 抛出 type-error
(check-catch 'type-error (json->pi-tool-result "bad"))
(check-catch 'type-error (json->pi-tool-result 42))

;; 4. 空 JSON 对象返回默认 pi-tool-result
(let ((r-empty (json->pi-tool-result '(()))))
  (check-true (pi-tool-result? r-empty))
  (check (pi-tool-result-content r-empty) => #())
  (check (pi-tool-result-is-error r-empty) => #f)
) ;let

(check-report)
