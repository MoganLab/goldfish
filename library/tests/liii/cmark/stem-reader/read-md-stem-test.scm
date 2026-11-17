;; read-md-stem
;; 读取 md.stem 资源文本，并返回完整的 `(md-stem ...)` 文档。
;;
;; 语法
;; ----
;; (read-md-stem [input-port])
;;
;; 参数
;; ----
;; [input-port] : any
;; 输入参数。
;;
;; 返回值
;; ----
;; any
;; 返回操作结果。

(import (scheme base))
(import (liii check))
(import (liii cmark stem-reader))

(check-set-mode! 'report-failed)

(check-true (procedure? read-md-stem))

(let ((doc (read-md-stem (open-input-string "(p \"正文\")\n(h1 \"标题\")\n"))))
  (check doc => '(md-stem (p "正文") (h1 "标题")))
) ;let

(let ((doc (read-md-stem (open-input-string "(code-block #\"scheme\"\n(define x 1)\n(display x)\n\"scheme\") ;code-block\n"
                         ) ;open-input-string
           ) ;read-md-stem
      ) ;doc
     ) ;
  (check doc => '(md-stem (code-block "scheme" "(define x 1)\n(display x)")))
) ;let

(let ((doc (read-md-stem (open-input-string "(p \"前文\")\n(code-block #\"\"\nline 1\nline 2\n\"\") ;code-block\n"
                         ) ;open-input-string
           ) ;read-md-stem
      ) ;doc
     ) ;
  (check doc => '(md-stem (p "前文") (code-block "" "line 1\nline 2")))
) ;let

(let ((doc (read-md-stem (open-input-string "(p \"包含\" (code \"git status\") \"的段落\")\n")
           ) ;read-md-stem
      ) ;doc
     ) ;
  (check doc => '(md-stem (p "包含" (code "git status") "的段落")))
) ;let

(let ((doc (read-md-stem (open-input-string "(p \"第一行\" (soft-break) \"第二行\" (line-break) \"第三行\")\n"
                         ) ;open-input-string
           ) ;read-md-stem
      ) ;doc
     ) ;
  (check doc
    =>
    '(md-stem (p "第一行" (soft-break) "第二行" (line-break) "第三行"))
  ) ;check
) ;let

(let ((doc (read-md-stem (open-input-string "(blockquote (p \"引用\") (blockquote (p \"内层\")))\n")
           ) ;read-md-stem
      ) ;doc
     ) ;
  (check doc => '(md-stem (blockquote (p "引用") (blockquote (p "内层")))))
) ;let

(let ((doc (read-md-stem (open-input-string "(bullet-list tight (item (p \"第一项\")) (item (blockquote (p \"第二项\"))))\n"
                         ) ;open-input-string
           ) ;read-md-stem
      ) ;doc
     ) ;
  (check doc
    =>
    '(md-stem (bullet-list tight
                (item (p "第一项"))
                (item (blockquote (p "第二项")))))
  ) ;check
) ;let

(let ((doc (read-md-stem (open-input-string "(ordered-list 3 period tight (item (p \"第三项\")) (item (ordered-list 1 one-paren loose (item (p \"内层\")))))\n"
                         ) ;open-input-string
           ) ;read-md-stem
      ) ;doc
     ) ;
  (check doc
    =>
    '(md-stem (ordered-list 3
                period
                tight
                (item (p "第三项"))
                (item (ordered-list 1 one-paren loose (item (p "内层"))))))
  ) ;check
) ;let

(let ((doc (read-md-stem (open-input-string "(p \"前文\")\n(thematic-break)\n(p \"后文\")\n")
           ) ;read-md-stem
      ) ;doc
     ) ;
  (check doc => '(md-stem (p "前文") (thematic-break) (p "后文")))
) ;let

(let ((doc (read-md-stem (open-input-string "(table (align default right) (thead (tr (th \"Name\") (th \"Value\"))) (tbody (tr (td \"A\") (td \"1\"))))\n"
                         ) ;open-input-string
           ) ;read-md-stem
      ) ;doc
     ) ;
  (check doc
    =>
    '(md-stem (table (align default right)
                (thead (tr (th "Name") (th "Value")))
                (tbody (tr (td "A") (td "1")))))
  ) ;check
) ;let

(let ((doc (read-md-stem (open-input-string "(p \"前文\" (raw-inline \"html\" \"<span>\") \"后文\")\n"
                         ) ;open-input-string
           ) ;read-md-stem
      ) ;doc
     ) ;
  (check doc => '(md-stem (p "前文" (raw-inline "html" "<span>") "后文")))
) ;let

(let ((doc (read-md-stem (open-input-string "(p \"公式\" (math-inline \"x^2\") \"结束\")\n")
           ) ;read-md-stem
      ) ;doc
     ) ;
  (check doc => '(md-stem (p "公式" (math-inline "x^2") "结束")))
) ;let

(let ((doc (read-md-stem (open-input-string "(p \"前文\" (strikeout \"删除\") \"后文\")\n")
           ) ;read-md-stem
      ) ;doc
     ) ;
  (check doc => '(md-stem (p "前文" (strikeout "删除") "后文")))
) ;let

(let ((doc (read-md-stem (open-input-string "(p (emoji \"smile\") \" 继续\")\n"))))
  (check doc => '(md-stem (p (emoji "smile") " 继续")))
) ;let

(let ((doc (read-md-stem (open-input-string "(p (link \"https://example.com\" \"\" \"a \" (raw-inline \"html\" \"<span>\") \"b\") (image \"logo.png\" \"\" \"x \" (raw-inline \"html\" \"<span>\") \"y\"))\n"
                         ) ;open-input-string
           ) ;read-md-stem
      ) ;doc
     ) ;
  (check doc
    =>
    '(md-stem (p (link "https://example.com"
                   ""
                   "a "
                   (raw-inline "html" "<span>")
                   "b")
                (image "logo.png" "" "x " (raw-inline "html" "<span>") "y")))
  ) ;check
) ;let

(let ((doc (read-md-stem (open-input-string "(bullet-list tight (item unchecked (p \"待办\")) (item checked (p \"完成\")))\n"
                         ) ;open-input-string
           ) ;read-md-stem
      ) ;doc
     ) ;
  (check doc
    =>
    '(md-stem (bullet-list tight
                (item unchecked (p "待办"))
                (item checked (p "完成"))))
  ) ;check
) ;let

(let ((doc (read-md-stem (open-input-string "(raw-block \"html\" \"<div>\\ncontent\\n</div>\")\n")
           ) ;read-md-stem
      ) ;doc
     ) ;
  (check doc => '(md-stem (raw-block "html" "<div>\ncontent\n</div>")))
) ;let

(let ((doc (read-md-stem (open-input-string "(math-block \"\\\\int_0^1 x^2\\\\,dx\")\n"))
      ) ;doc
     ) ;
  (check doc => '(md-stem (math-block "\\int_0^1 x^2\\,dx")))
) ;let

(let ((doc (read-md-stem (open-input-string "(alert note (p \"提示内容\"))\n"))))
  (check doc => '(md-stem (alert note (p "提示内容"))))
) ;let

(check-report)
