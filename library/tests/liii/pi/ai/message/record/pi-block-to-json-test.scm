;; pi-block->json
;; 将 content block record 转换为符合 (liii json) 规范的 Scheme-JSON 对象。
;;
;; 语法
;; ----
;; (pi-block->json blk)
;;
;; 参数
;; ----
;; blk : any?
;; <pi-text-block>、<pi-thinking-block>、<pi-tool-call-block>、<pi-image-block> 或 alist。
;;
;; 返回值
;; ----
;; any?
;; 可直接用于 json->string 或 json-ref 的 Scheme-JSON 对象。

(import (liii check) (liii json) (liii pi ai message record))

(check-set-mode! 'report-failed)

(check-true (procedure? pi-block->json))

;; 1. text block
(let ((j (pi-block->json (pi-text-block "hello" "sig-1"))))
  (check (json-ref j "type") => "text")
  (check (json-ref j "text") => "hello")
  (check (json-ref j "textSignature") => "sig-1")
  (check (string->json (json->string j)) => j)
) ;let

;; 2. thinking block
(let ((j (pi-block->json (pi-thinking-block "reasoning" "tsig"))))
  (check (json-ref j "type") => "thinking")
  (check (json-ref j "thinking") => "reasoning")
  (check (json-ref j "thinkingSignature") => "tsig")
) ;let

;; 3. toolCall block：空 arguments 规范化为 JSON 空对象 (())，符号键规范化为字符串键
(let ((j1 (pi-block->json (pi-tool-call-block "c1" "bash" '())))
      (j2 (pi-block->json (pi-tool-call-block "c2" "read" '((path . "a.scm")) "th-sig")))
     ) ;
  (check (json-ref j1 "type") => "toolCall")
  (check (json-ref j1 "id") => "c1")
  (check (json-ref j1 "name") => "bash")
  (check (json-ref j1 "arguments") => '(()))
  (check (json->string j1) => "{\"type\":\"toolCall\",\"id\":\"c1\",\"name\":\"bash\",\"arguments\":{}}")
  (check (json-ref j2 "arguments" "path") => "a.scm")
  (check (json-ref j2 "thoughtSignature") => "th-sig")
) ;let

;; 4. image block
(let ((j (pi-block->json (pi-image-block "base64data" "image/png"))))
  (check (json-ref j "type") => "image")
  (check (json-ref j "data") => "base64data")
  (check (json-ref j "mimeType") => "image/png")
) ;let

(check-report)
