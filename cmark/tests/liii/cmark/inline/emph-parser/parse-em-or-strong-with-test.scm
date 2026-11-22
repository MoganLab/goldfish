;; parse-em-or-strong-with
;; 使用指定的行内解析回调函数解析 * / _ 强调与粗体。
;;
;; 语法
;; ----
;; (parse-em-or-strong-with str start refmap parse-inlines-fn parse-link-fn parse-image-fn)
;;
;; 参数
;; ----
;; str : string
;; 输入字符串。
;; start : integer
;; 起始位置。
;; refmap : reference-map 或 #f
;; 引用映射。
;; parse-inlines-fn : procedure
;; 行内解析回调。
;; parse-link-fn : procedure
;; 链接解析回调。
;; parse-image-fn : procedure
;; 图片解析回调。
;;
;; 返回值
;; ----
;; pair
;; 返回 (nodes-list . next-pos)。

(import (scheme base))
(import (liii check))
(import (liii cmark inline emph-parser))
(import (liii cmark node))
(import (liii cmark inline link-parser))

(check-set-mode! 'report-failed)

(let ((res
        (parse-em-or-strong-with "*hi*"
          0
          #f
          (lambda (s r) (list (make-cmark-text s)))
          (lambda args #f)
          (lambda args #f)
        ) ;parse-em-or-strong-with
      ) ;res
     ) ;
  (check (cmark-node-type (cadar res)) => 'emph)
  (check (cdr res) => 4)
) ;let

(check-report)
