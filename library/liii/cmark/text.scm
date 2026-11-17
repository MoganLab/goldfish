
(define-library (liii cmark text)
  (import (scheme base))
  (import (liii ascii))
  (import (liii string))
  ;; re-export 契约：string-join / string-index / string-trim-left / string-trim-right
  ;; 是 (liii string) 绑定的直接 re-export（无本地定义）；
  ;; 缺任一导出名则本库加载即失败，改动 (liii string) 导出名时需同步此处。
  (export string-prefix? string-prefix-at? string-join string-split-lines
    string-index string-trim-left string-trim-right string-trim find-newline
    strip-trailing-carriage-return count-leading-spaces char-run-length
    skip-ascii-whitespace assoc-ref-default count-leading-indent
    ascii-whitespace? string-index-of char-at? string-at? prepend-rev
    stable-sort restore-math-greek-herk
  ) ;export
  (begin
    ;; ---- re-export 契约 ----
    ;; 直接 re-export (liii string)：string-join / string-index /
    ;;   string-trim-left / string-trim-right
    ;; 适配 re-export：
    ;;   string-prefix? ← string-starts?（参数顺序相反）
    ;;   string-trim    ← string-trim-both（SRFI-13 的 string-trim 只去左边，
    ;;                    本项目 string-trim 语义是去两边）
    ;; 全部基于 ascii-whitespace?（codepoint 9-13 与空格 32），
    ;; 行为与旧 ASCII 版一致（含 NBSP / 全角空格均不修剪）。
    (define (string-prefix? prefix str)
      (string-starts? str prefix)
    ) ;define

    ;; 在字符串 str 的 pos 位置检查是否以 prefix 开头（与位置无关的通用工具）
    (define (string-prefix-at? str pos prefix)
      (let ((prefix-len (string-length prefix)) (len (string-length str)))
        (and (<= (+ pos prefix-len) len)
          (string=? (substring str pos (+ pos prefix-len)) prefix)
        ) ;and
      ) ;let
    ) ;define

    (define (string-trim str)
      (string-trim-both str)
    ) ;define

    (define (find-newline str start)
      (string-index str #\newline start)
    ) ;define

    (define (strip-trailing-carriage-return str)
      (let ((len (string-length str)))
        (if
          (and (> len 0) (char=? (string-ref str (- len 1)) #\return))
          (substring str 0 (- len 1))
          str
        ) ;if
      ) ;let
    ) ;define

    ;; 按行切分：丢弃末尾空行（与 SRFI-13 string-split 不同——
    ;; string-split 保留末尾空串，markdown 解析不需要，故保留自定义实现）
    (define (string-split-lines str)
      (let ((len (string-length str)))
        (let loop
          ((start 0) (lines '()))
          (if (> start len)
            (reverse lines)
            (let ((newline-pos (find-newline str start)))
              (if newline-pos
                (loop (+ newline-pos 1)
                  (cons (strip-trailing-carriage-return (substring str start newline-pos)) lines)
                ) ;loop
                (if (= start len)
                  (reverse lines)
                  (reverse (cons (strip-trailing-carriage-return (substring str start len)) lines)
                  ) ;reverse
                ) ;if
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (count-leading-spaces str)
      (let ((len (string-length str)))
        (let loop
          ((i 0))
          (cond ((>= i len) i)
                ((char=? (string-ref str i) #\space) (loop (+ i 1)))
                (else i)
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    ;; 从 start 起统计连续字符 ch 的长度（count-run / count-backticks 统一版）
    (define (char-run-length str start ch)
      (let ((len (string-length str)))
        (let loop
          ((i start))
          (if (and (< i len) (char=? (string-ref str i) ch)) (loop (+ i 1)) (- i start))
        ) ;let
      ) ;let
    ) ;define

    ;; 从 start 起跳过 ascii-whitespace? 字符，返回第一个非空白位置（skip-spaces 统一版）
    (define (skip-ascii-whitespace str start)
      (let ((len (string-length str)))
        (let loop
          ((i start))
          (if (and (< i len) (ascii-whitespace? (string-ref str i))) (loop (+ i 1)) i)
        ) ;let
      ) ;let
    ) ;define

    ;; alist 取 key 带默认值（node-data-ref / markdown-block-data-ref 统一版，见 1027 B5）
    (define (assoc-ref-default alist key default)
      (let ((cell (assoc key alist)))
        (if cell (cdr cell) default)
      ) ;let
    ) ;define

    ;; 数前导空格与 tab（区别于 count-leading-spaces 只数空格）
    (define (count-leading-indent line)
      (let loop
        ((i 0) (n (string-length line)))
        (if (>= i n)
          i
          (let ((ch (string-ref line i)))
            (if (or (char=? ch #\space) (char=? ch #\tab)) (loop (+ i 1) n) i)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; s 从 pos 起查找子串 sub（返回起始索引或 #f）
    (define (string-index-of s sub start)
      (if (string-null? sub)
        (and (>= start 0) (<= start (string-length s)) start)
        (string-position sub s start)
      ) ;if
    ) ;define

    ;; s 位置 pos 是字符 ch（且未越界）
    (define (char-at? s pos len ch)
      (and (< pos len) (char=? (string-ref s pos) ch))
    ) ;define

    ;; s 从 pos 起匹配子串 sub（越界返回 #f）
    (define (string-at? s pos sub)
      (and (>= pos 0)
        (<= (+ pos (string-length sub)) (string-length s))
        (string=? sub (substring s pos (+ pos (string-length sub))))
      ) ;and
    ) ;define

    ;; 返回 reverse(l) ++ acc（尾共享 acc，不拷贝 acc，仅 O(len l)）。
    ;; 用于在反复拼接增长结果时避免 append 的深拷贝（goldfish append 会拷贝全部参数 → O(n²)）。
    (define (prepend-rev l acc)
      (let add
        ((s l) (a acc))
        (if (null? s) a (add (cdr s) (cons (car s) a)))
      ) ;let
    ) ;define

    ;; 稳定归并排序：按比较器 lt 对列表排序（O(n log n)，稳定）。
    ;; lt 为严格全序谓词；(lt a b) 真表示 a 排在 b 前。输入乱序/反序均 O(n log n)。
    ;; 不用 srfi-132 list-sort：goldfish 该实现为弱排序，乱序输入会退化 O(n²)。
    (define (stable-sort xs lt)
      (define (merge a b)
        ;; 稳定合并两个已排序列表；结果反向累积，返回后 reverse。
        (let loop
          ((a a) (b b) (acc '()))
          (cond ((null? a) (prepend-rev b acc))
                ((null? b) (prepend-rev a acc))
                ((lt (car b) (car a)) (loop a (cdr b) (cons (car b) acc)))
                (else (loop (cdr a) b (cons (car a) acc)))
          ) ;cond
        ) ;let
      ) ;define
      (define (split-half x n)
        ;; 返回 (cons 前 n 个 剩余)
        (let loop
          ((x x) (k n) (first '()))
          (if (or (= k 0) (null? x))
            (cons (reverse first) x)
            (loop (cdr x) (- k 1) (cons (car x) first))
          ) ;if
        ) ;let
      ) ;define
      (let rec
        ((x xs))
        (if (or (null? x) (null? (cdr x)))
          x
          (let* ((n (quotient (length x) 2))
                 (parts (split-half x n))
                 (left (rec (car parts)))
                 (right (rec (cdr parts)))
                ) ;
            (reverse (merge left right))
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 还原数学容器内被 utf8->herk 转为 <#3C6>/<#3D5>/<#3D6> 的希腊字母实体为
    ;; TeXmacs 命名符号（<varphi>/<phi>/<varpi>），保持与 cmmi 数学字体字形一致
    (define (restore-math-greek-herk s)
      (let* ((s1 (string-replace s "<#3C6>" "<varphi>"))
             (s2 (string-replace s1 "<#03C6>" "<varphi>"))
             (s3 (string-replace s2 "<#3D5>" "<phi>"))
             (s4 (string-replace s3 "<#03D5>" "<phi>"))
             (s5 (string-replace s4 "<#3D6>" "<varpi>"))
            ) ;
        (string-replace s5 "<#03D6>" "<varpi>")
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
