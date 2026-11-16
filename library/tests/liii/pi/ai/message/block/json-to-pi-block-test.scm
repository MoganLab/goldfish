;; json->pi-block
;; 将 (liii json) 解析出的 Scheme-JSON 对象转换为 content block record。
;;
;; 语法
;; ----
;; (json->pi-block obj)
;;
;; 参数
;; ----
;; obj : any?
;; Scheme-JSON alist 或已有的 pi-block record。
;;
;; 返回值
;; ----
;; any?
;; 对应的 <pi-text-block>、<pi-thinking-block>、<pi-tool-call-block>、<pi-image-block> 或 #f。

(import (liii check) (liii json) (liii pi ai message))

(check-set-mode! 'report-failed)

(check-true (procedure? json->pi-block))

;; 1. 非标准 JSON 对象（含 pi-block record）抛出 type-error
(let ((b (pi-text-block "hi" "s1")))
  (check-catch 'type-error (json->pi-block b))
) ;let

;; 2. text / thinking / toolCall / image round-trip
(let* ((b1 (pi-text-block "hello" "sig-1"))
       (r1
         (json->pi-block (string->json (json->string (pi-block->json b1))))
       ) ;r1
      ) ;
  (check-true (pi-text-block? r1))
  (check (pi-text-block-text r1) => "hello")
  (check (pi-text-block-signature r1) => "sig-1")
) ;let*

(let* ((b2 (pi-thinking-block "think" "tsig"))
       (r2
         (json->pi-block (string->json (json->string (pi-block->json b2))))
       ) ;r2
      ) ;
  (check-true (pi-thinking-block? r2))
  (check (pi-thinking-block-thinking r2) => "think")
  (check (pi-thinking-block-thinking-signature r2) => "tsig")
) ;let*

(let* ((b3 (pi-tool-call-block "c1" "read" '(("path" . "a.scm")) "th-sig"))
       (r3
         (json->pi-block (string->json (json->string (pi-block->json b3))))
       ) ;r3
       (b3-empty (pi-tool-call-block "c2" "bash" '()))
       (r3-empty
         (json->pi-block (string->json (json->string (pi-block->json b3-empty))))
       ) ;r3-empty
      ) ;
  (check-true (pi-tool-call-block? r3))
  (check (pi-tool-call-block-id r3) => "c1")
  (check (pi-tool-call-block-name r3) => "read")
  (check (json-ref (pi-tool-call-block-arguments r3) "path") => "a.scm")
  (check (pi-tool-call-block-thought-signature r3) => "th-sig")
  (check (pi-tool-call-block-arguments r3-empty) => '(()))
) ;let*

(let* ((b4 (pi-image-block "base64" "image/jpeg"))
       (r4
         (json->pi-block (string->json (json->string (pi-block->json b4))))
       ) ;r4
      ) ;
  (check-true (pi-image-block? r4))
  (check (pi-image-block-data r4) => "base64")
  (check (pi-image-block-mime-type r4) => "image/jpeg")
) ;let*

;; 3. 未知类型返回 #f，非标准 JSON 对象（含 symbol-key alist）抛出 type-error
(check-false (json->pi-block '(("type" . "unknown"))))
(check-catch 'type-error (json->pi-block '((type . "text") (text . "hi"))))
(check-catch 'type-error (json->pi-block "not-a-block"))

(check-report)
