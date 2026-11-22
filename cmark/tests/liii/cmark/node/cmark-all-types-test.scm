;; cmark-all-types
;; 返回 cmark 支持的全部节点类型，并配合类型谓词使用。
;;
;; 语法
;; ----
;; (cmark-all-types)
;;
;;
;; 参数
;; ----
;; 无参数。
;; 返回值
;; ----
;; list
;; 返回块级类型与行内类型拼接得到的完整列表。

(import (liii check) (liii list) (liii cmark node))

(check-set-mode! 'report-failed)

(check-true (cmark-block-type? 'document))
(check-true (cmark-block-type? 'paragraph))
(check-true (cmark-block-type? 'heading))
(check-true (cmark-block-type? 'table))
(check-true (cmark-block-type? 'alert))
(check-false (cmark-block-type? 'text))
(check-false (cmark-block-type? 'link))
(check-true (cmark-block-type? 'math_block))

(check-true (cmark-inline-type? 'text))
(check-true (cmark-inline-type? 'emph))
(check-true (cmark-inline-type? 'strong))
(check-true (cmark-inline-type? 'emoji))
(check-true (cmark-inline-type? 'strikeout))
(check-false (cmark-inline-type? 'paragraph))
(check-false (cmark-inline-type? 'document))
(check-true (cmark-inline-type? 'math_inline))

;; 测试 cmark-all-types 的构成：应为块级与行内类型拼接，无重复
(let ((all (cmark-all-types))
      (blocks *cmark-block-types*)
      (inlines *cmark-inline-types*)
     ) ;
  ;; 长度应为两者之和
  (check (length all) => (+ (length blocks) (length inlines)))
  ;; 每个块级类型都应在 all 中
  (for-each
    (lambda (t) (check-true (if (member t all) #t #f)))
    blocks
  ) ;for-each
  ;; 每个行内类型都应在 all 中
  (for-each
    (lambda (t) (check-true (if (member t all) #t #f)))
    inlines
  ) ;for-each
  ;; 块级与行内类型不应有交集
  (check
    (length (filter (lambda (t) (member t inlines)) blocks))
    =>
    0
  ) ;check
) ;let

;; 测试已知类型的存在性（替代硬编码长度，确保新增类型时此处自动通过）
(let ((expected-types '(document block_quote alert list item code_block
                         html_block paragraph heading thematic_break table
                         math_block text softbreak linebreak code emoji
                         html_inline custom_inline emph strong strikeout link
                         image math_inline custom_block)
      ) ;expected-types
     ) ;
  (for-each
    (lambda (t) (check-true (if (member t (cmark-all-types)) #t #f)))
    expected-types
  ) ;for-each
) ;let

(check-report)
