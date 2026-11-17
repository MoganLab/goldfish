
(set! *load-path* (cons "markdown/goldfish" *load-path*))

(import (liii check) (liii cmark inline math) (liii cmark node))

(check-set-mode! 'report-failed)

;; parse-math-inline
;; 从指定位置解析一段行内数学公式。
;;
;; 语法
;; ----
;; (parse-math-inline str start)
;;
;; 返回值
;; ----
;; pair
;; 返回 (node . next-pos)，其中 node 可能是 math_inline 或 text。

(let ((result (parse-math-inline "$E=mc^2$" 0)))
  (check (cmark-node-type (car result)) => 'math_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    "E=mc^2"
  ) ;check
  (check (cdr result) => 8)
) ;let

;; 未闭合 $ 只消费起始 $，不吞剩余（CommonMark Example 14 未闭合 fallback）
(let ((result (parse-math-inline "$unclosed" 0)))
  (check (cmark-node-type (car result)) => 'text)
  (check (cdr result) => 1)
) ;let

(let ((result (parse-math-inline "" 0)))
  (check (cmark-node-type (car result)) => 'text)
  (check (cdr result) => 0)
) ;let

;; $$...$$ 同行：kramdown 双用语法（wiki: kramdown/Typora/Deckset 等），
;; 内容非空识别为行内公式
(let ((result (parse-math-inline "$$inline$$" 0)))
  (check (cmark-node-type (car result)) => 'math_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    "inline"
  ) ;check
  (check (cdr result) => 10)
) ;let

;; 未闭合或空内容 $$：按文本消费一个 $（避免空 math-inline）
(let ((result (parse-math-inline "$$x" 0)))
  (check (cmark-node-type (car result)) => 'text)
  (check (cdr result) => 1)
) ;let

;; 货币假阳性：$10 and $20、$10-$20 不识别（closing $ 后数字的启发式）
(let ((result (parse-math-inline "$10 and $20" 0)))
  (check (cmark-node-type (car result)) => 'text)
  (check (cdr result) => 1)
) ;let
(let ((result (parse-math-inline "$10-$20" 0)))
  (check (cmark-node-type (car result)) => 'text)
  (check (cdr result) => 1)
) ;let

;; 行内公式紧跟数字：开定界符后为非数字，闭合定界符紧跟数字正常识别
(let ((result (parse-math-inline "$^{*}$17" 0)))
  (check (cmark-node-type (car result)) => 'math_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    "^{*}"
  ) ;check
  (check (cdr result) => 6)
) ;let
(let ((result (parse-math-inline "$x^2$1" 0)))
  (check (cmark-node-type (car result)) => 'math_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    "x^2"
  ) ;check
  (check (cdr result) => 5)
) ;let

;; 内侧空格：$ x$ / $x $ 识别为数学，空格保留在内容中（7.1 Math 15/16 定稿）
(let ((result (parse-math-inline "$ x$" 0)))
  (check (cmark-node-type (car result)) => 'math_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    " x"
  ) ;check
  (check (cdr result) => 4)
) ;let
(let ((result (parse-math-inline "$x $" 0)))
  (check (cmark-node-type (car result)) => 'math_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    "x "
  ) ;check
  (check (cdr result) => 4)
) ;let

;; 边界：$$$$ 空内容按文本
(let ((result (parse-math-inline "$$$$" 0)))
  (check (cmark-node-type (car result)) => 'text)
  (check (cdr result) => 1)
) ;let

;; 边界：$$ 内容含单个 $（如 $a $ b$）整体为行内公式
(let ((result (parse-math-inline "$$a $ b$$" 0)))
  (check (cmark-node-type (car result)) => 'math_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    "a $ b"
  ) ;check
  (check (cdr result) => 9)
) ;let

;; 边界：$a$$b$ 连续两个公式（第一个闭合后继续解析）
(let ((result (parse-math-inline "$a$$b$" 0)))
  (check (cmark-node-type (car result)) => 'math_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    "a"
  ) ;check
  (check (cdr result) => 3)
) ;let

;; 边界：$$ $$ 内容为空格仍识别（内容非空即行内公式）
(let ((result (parse-math-inline "$$ $$" 0)))
  (check (cmark-node-type (car result)) => 'math_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    " "
  ) ;check
  (check (cdr result) => 5)
) ;let

;; \(...\) 定界：行内公式
(let ((result (parse-math-inline "\\(E=mc^2\\)" 0)))
  (check (cmark-node-type (car result)) => 'math_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    "E=mc^2"
  ) ;check
  (check (cdr result) => 10)
) ;let

;; \(...\) 未闭合：回退为转义字面量 (（消费 \( 两个字符）
(let ((result (parse-math-inline "\\(unclosed" 0)))
  (check (cmark-node-type (car result)) => 'text)
  (check
    (cdr (assoc 'content (cmark-node-data (car result))))
    =>
    "("
  ) ;check
  (check (cdr result) => 2)
) ;let

;; \(\) 空内容：回退为转义字面量
(let ((result (parse-math-inline "\\(\\)" 0)))
  (check (cmark-node-type (car result)) => 'text)
  (check (cdr result) => 2)
) ;let

;; 内容含转义 \\)：\\) 不作闭合，继续向后找未转义的 \\)
(let ((result (parse-math-inline "\\(a\\\\)b\\)" 0)))
  (check (cmark-node-type (car result)) => 'math_inline)
  (check
    (cdr (assoc 'literal (cmark-node-data (car result))))
    =>
    "a\\\\)b"
  ) ;check
) ;let

(check-report)
