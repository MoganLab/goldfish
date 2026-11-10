;; pi-transcript-normalize-block
;; 内容块净化：跨模型时丢弃思维链并剥离私有签名。
;;
;; 语法
;; ----
;; (pi-transcript-normalize-block blk same-model?)
;;
;; 参数
;; ----
;; blk : any?
;; 内容块，支持 record（pi-block?）与 alist 两种形态。
;; same-model? : boolean
;; 该消息是否与请求目标同源；#f 表示异构模型。
;;
;; 返回值
;; ----
;; alist? 或 #f
;; 净化后的内容块关联表；思维链被丢弃时返回 #f。

(import (liii check)
  (liii pi ai message)
  (liii pi ai transcript cross-model)
) ;import

(check-set-mode! 'report-failed)

(define (blk-type b) (cdr (assoc "type" b)))

;; 异构模型：thinking 块被丢弃
(check (pi-transcript-normalize-block
         '(("type" . "thinking") ("thinking" . "异构推理") ("thinkingSignature" . "SIG"))
         #f
       ) => #f)

;; 异构模型：text 块的私有签名被剥离，正文保留
(let ((out (pi-transcript-normalize-block
             '(("type" . "text") ("text" . "正文") ("textSignature" . "SIG"))
             #f
           ) ;pi-transcript-normalize-block
          ) ;out
         ) ;
  (check (blk-type out) => "text")
  (check (cdr (assoc "text" out)) => "正文")
  (check (assoc "textSignature" out) => #f)
) ;let

;; 异构模型：toolCall 块的 thoughtSignature 被剥离，工具信息保留
(let ((out (pi-transcript-normalize-block
             '(("type" . "toolCall") ("id" . "c1") ("name" . "read") ("thoughtSignature" . "SIG"))
             #f
           ) ;pi-transcript-normalize-block
          ) ;out
         ) ;
  (check (blk-type out) => "toolCall")
  (check (cdr (assoc "id" out)) => "c1")
  (check (cdr (assoc "name" out)) => "read")
  (check (assoc "thoughtSignature" out) => #f)
) ;let

;; 同模型：思维链、文本签名与工具签名全部保留
(let ((out (pi-transcript-normalize-block
             '(("type" . "thinking") ("thinking" . "同源推理") ("thinkingSignature" . "SIG"))
             #t
           ) ;pi-transcript-normalize-block
          ) ;out
         ) ;
  (check (blk-type out) => "thinking")
  (check (cdr (assoc "thinking" out)) => "同源推理")
  (check (cdr (assoc "thinkingSignature" out)) => "SIG")
) ;let

(let ((out (pi-transcript-normalize-block
             '(("type" . "toolCall") ("id" . "c2") ("thoughtSignature" . "SIG2"))
             #t
           ) ;pi-transcript-normalize-block
          ) ;out
         ) ;
  (check (cdr (assoc "thoughtSignature" out)) => "SIG2")
) ;let

;; 同模型：空思维链（无正文无签名）被丢弃
(check (pi-transcript-normalize-block '(("type" . "thinking") ("thinking" . "")) #t) => #f)

;; record 形态的内容块同样支持
(let ((out (pi-transcript-normalize-block
             (pi-tool-call-block "c3" "bash" '() "SIG3")
             #f
           ) ;pi-transcript-normalize-block
          ) ;out
         ) ;
  (check (blk-type out) => "toolCall")
  (check (cdr (assoc "id" out)) => "c3")
  (check (assoc "thoughtSignature" out) => #f)
) ;let

(check-report)
