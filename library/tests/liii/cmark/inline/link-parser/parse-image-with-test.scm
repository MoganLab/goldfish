;; parse-image-with
;; 使用指定的行内解析回调解析图片结构。
;;
;; 语法
;; ----
;; (parse-image-with str start refmap parse-inlines-fn)
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
;;
;; 返回值
;; ----
;; pair
;; 返回 (node . next-pos)。

(import (scheme base))
(import (liii check))
(import (liii cmark inline link-parser))
(import (liii cmark node))
(import (liii cmark block reference))

(check-set-mode! 'report-failed)

(let ((res
        (parse-image-with "![alt](img.png)"
          0
          #f
          (lambda (s r) (list (make-cmark-text s)))
        ) ;parse-image-with
      ) ;res
     ) ;
  (check (cmark-node-type (car res)) => 'image)
  (check (cdr res) => 15)
) ;let

(check-report)
