;; gemini-assistant-block->part
;; 将 assistant 内容块转换为 Gemini part，同模型时回放 thought signature。
;;
;; 语法
;; ----
;; (gemini-assistant-block->part blk same-model?)
;;
;; 参数
;; ----
;; blk : alist | record
;; assistant 内容块，可以是内部 record 块或 alist 形态。
;; same-model? : boolean
;; 当前消息与请求是否来自同一 provider 与 model；为 #f 时不回传签名。
;;
;; 返回值
;; ----
;; alist | #f
;; Gemini part（alist）；块类型无法识别或内容为空时返回 #f。
;;
;; 说明
;; ----
;; text 块携带 textSignature、thinking 块携带 thinkingSignature、
;; toolCall 块携带 thoughtSignature；签名合法且同模型时才回放。
;; 跨模型时 thinking 块直接过滤（返回 #f），不发射为文本。

(import (liii check)
  (liii pi ai api google-generative-ai thought-signature)
  (liii pi ai message)
) ;import

(check-set-mode! 'report-failed)

;; text 块：无签名
(check (gemini-assistant-block->part '(("type" . "text") ("text" . "visible")) #t)
  =>
  '(("text" . "visible"))
) ;check

;; text 块：同模型回放签名
(check (gemini-assistant-block->part '(("type" . "text")
                                       ("text" . "visible")
                                       ("textSignature" . "QUJD"))
         #t
       ) ;gemini-assistant-block->part
  =>
  '(("thoughtSignature" . "QUJD") ("text" . "visible"))
) ;check

;; text 块：跨模型不回放签名
(check (gemini-assistant-block->part '(("type" . "text")
                                       ("text" . "visible")
                                       ("textSignature" . "QUJD"))
         #f
       ) ;gemini-assistant-block->part
  =>
  '(("text" . "visible"))
) ;check

;; text 块：空文本且无签名 → 丢弃
(check (gemini-assistant-block->part '(("type" . "text") ("text" . "")) #t)
  =>
  #f
) ;check

;; text 块：空文本但带签名 → 保留
(check (gemini-assistant-block->part '(("type" . "text")
                                       ("text" . "")
                                       ("textSignature" . "QUJD"))
         #t
       ) ;gemini-assistant-block->part
  =>
  '(("thoughtSignature" . "QUJD") ("text" . ""))
) ;check

;; text 块：非法签名不回放
(check (gemini-assistant-block->part '(("type" . "text")
                                       ("text" . "visible")
                                       ("textSignature" . "ab!!"))
         #t
       ) ;gemini-assistant-block->part
  =>
  '(("text" . "visible"))
) ;check

;; thinking 块：同模型带 thought 标记
(check (gemini-assistant-block->part '(("type" . "thinking")
                                       ("thinking" . "reasoning..."))
         #t
       ) ;gemini-assistant-block->part
  =>
  '(("thought" . #t) ("text" . "reasoning..."))
) ;check

;; thinking 块：同模型回放签名
(check (gemini-assistant-block->part '(("type" . "thinking")
                                       ("thinking" . "reasoning...")
                                       ("thinkingSignature" . "QUJD"))
         #t
       ) ;gemini-assistant-block->part
  =>
  '(("thoughtSignature" . "QUJD") ("thought" . #t) ("text" . "reasoning..."))
) ;check

;; thinking 块：跨模型丢弃（返回 #f）
(check (gemini-assistant-block->part '(("type" . "thinking")
                                       ("thinking" . "reasoning...")
                                       ("thinkingSignature" . "QUJD"))
         #f
       ) ;gemini-assistant-block->part
  =>
  #f
) ;check

;; thinking 块：空内容且无签名 → 丢弃
(check (gemini-assistant-block->part '(("type" . "thinking") ("thinking" . "")) #t)
  =>
  #f
) ;check

;; thinking 块：空内容但带签名 → 保留
(check (gemini-assistant-block->part '(("type" . "thinking")
                                       ("thinking" . "")
                                       ("thinkingSignature" . "QUJD"))
         #t
       ) ;gemini-assistant-block->part
  =>
  '(("thoughtSignature" . "QUJD") ("thought" . #t) ("text" . ""))
) ;check

;; toolCall 块：转换为 functionCall
(check (gemini-assistant-block->part '(("type" . "toolCall")
                                       ("id" . "c1")
                                       ("name" . "read")
                                       ("arguments"))
         #t
       ) ;gemini-assistant-block->part
  =>
  '(("functionCall" ("name" . "read") ("args") ("id" . "c1")))
) ;check

;; toolCall 块：同模型回放 thoughtSignature
(check (gemini-assistant-block->part '(("type" . "toolCall")
                                       ("id" . "c1")
                                       ("name" . "read")
                                       ("arguments")
                                       ("thoughtSignature" . "QUJD"))
         #t
       ) ;gemini-assistant-block->part
  =>
  '(("thoughtSignature" . "QUJD")
    ("functionCall" ("name" . "read") ("args") ("id" . "c1")))
) ;check

;; toolCall 块：跨模型不回放 thoughtSignature
(check (gemini-assistant-block->part '(("type" . "toolCall")
                                       ("id" . "c1")
                                       ("name" . "read")
                                       ("arguments")
                                       ("thoughtSignature" . "QUJD"))
         #f
       ) ;gemini-assistant-block->part
  =>
  '(("functionCall" ("name" . "read") ("args") ("id" . "c1")))
) ;check

;; 未知块类型 → #f
(check (gemini-assistant-block->part '(("type" . "unknown")) #t) => #f)

;; record 形态：text 块
(check (gemini-assistant-block->part (pi-text-block "visible") #t)
  =>
  '(("text" . "visible"))
) ;check

;; record 形态：text 块带签名
(check (gemini-assistant-block->part (pi-text-block "visible" "QUJD") #t)
  =>
  '(("thoughtSignature" . "QUJD") ("text" . "visible"))
) ;check

;; record 形态：thinking 块带签名
(check (gemini-assistant-block->part (pi-thinking-block "think" "QUJD") #t)
  =>
  '(("thoughtSignature" . "QUJD") ("thought" . #t) ("text" . "think"))
) ;check

;; record 形态：toolCall 块带签名
(check (gemini-assistant-block->part (pi-tool-call-block "c1" "read" '() "QUJD") #t)
  =>
  '(("thoughtSignature" . "QUJD")
    ("functionCall" ("name" . "read") ("args") ("id" . "c1")))
) ;check

(check-report)
