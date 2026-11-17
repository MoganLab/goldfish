(define-library (liii cmark inline parser)
  (import (scheme base))
  (import (scheme char))
  (import (srfi srfi-1))
  (import (liii cmark text))
  (import (liii cmark inline char))
  (import (liii cmark inline emphasis))
  (import (liii cmark inline util))
  (import (liii cmark inline autolink))
  (import (liii cmark inline escape))
  (import (liii cmark inline emoji))
  (import (liii cmark inline html))
  (import (liii cmark block reference))
  (import (liii cmark inline math))
  (import (liii cmark node))
  (export make-cmark-inline-parser cmark-inline-parser?
    cmark-inline-parser-content cmark-inline-parser-set-content!
    cmark-inline-parser-pos cmark-inline-parser-set-pos!
    cmark-inline-parser-node cmark-inline-parser-set-node!
    cmark-inline-parser-new parse-text inline-code-start? parse-inline-code
    emph-start? parse-emph strong-start? parse-strong strikeout-start?
    parse-strikeout link-start? parse-link image-start? parse-image
    autolink-start? parse-autolink bare-autolink-start? parse-bare-autolink
    emoji-start? parse-emoji inline-html-start? parse-inline-html linebreak?
    parse-linebreak parse-inlines
  ) ;export
  (begin
    ;; bare autolink 全局开关（goldfish 扩展，非 CommonMark）：默认开启
    (define *bare-autolink-enabled* #t)
    (define (bare-autolink-enabled?)
      *bare-autolink-enabled*
    ) ;define

    (define-record-type <cmark-inline-parser>
      (make-cmark-inline-parser)
      cmark-inline-parser?
      (content cmark-inline-parser-content cmark-inline-parser-set-content!)
      (pos cmark-inline-parser-pos cmark-inline-parser-set-pos!)
      (node cmark-inline-parser-node cmark-inline-parser-set-node!)
    ) ;define-record-type
    (define (cmark-inline-parser-new)
      (let ((parser (make-cmark-inline-parser)))
        (cmark-inline-parser-set-content! parser "")
        (cmark-inline-parser-set-pos! parser 0)
        (cmark-inline-parser-set-node! parser #f)
        parser
      ) ;let
    ) ;define
    (define (parse-text str)
      (make-cmark-text str)
    ) ;define

    ;; 解析文本片段（§3.11）
    (define (parse-inline-text str pos next-special)
      (parse-text (substring str pos next-special))
    ) ;define
    (define (inline-code-start? str pos)
      (and (< pos (string-length str)) (char=? (string-ref str pos) #\`))
    ) ;define
    ;; 换行转空格；两端各剥一个空格，但内容全为空格时不剥（Example 121/145/334）
    (define (normalize-code-span content)
      (let* ((spaced
               (string-map (lambda (c) (if (char=? c #\newline) #\space c)) content)
             ) ;spaced
             (len (string-length spaced))
            ) ;
        (if
          (and (>= len 2)
            (char=? (string-ref spaced 0) #\space)
            (char=? (string-ref spaced (- len 1)) #\space)
            (not (all-spaces? spaced))
          ) ;and
          (substring spaced 1 (- len 1))
          spaced
        ) ;if
      ) ;let*
    ) ;define

    ;; 全部字符均为空格（code span 内容全空格时保留原样，Example 334）

    (define (parse-inline-code str start)
      (let* ((len (string-length str))
             (marker-len (char-run-length str start #\`))
             (content-start (+ start marker-len))
             (end-pos (find-matching-backticks str content-start marker-len))
            ) ;
        (if end-pos
          (cons (make-cmark-code (normalize-code-span (substring str content-start end-pos)))
            (+ end-pos marker-len)
          ) ;cons
          ;; 未闭合 code span 只消费起始反引号，继续解析后续（避免吞掉整个剩余串）
          (cons (make-cmark-text (substring str start content-start)) content-start)
        ) ;if
      ) ;let*
    ) ;define
    ;; CommonMark flanking 规则（§6.2，Example 352-398）
    ;; 字符类别判定基于 UTF-8 解码后的码点（字节寻址下的多字节字符须解码，Example 394/406）
    ;; UTF-8 解码：返回 (cons codepoint char-len)；非法或越界 #f
    ;; 行首/行尾计为空白（CommonMark §6.2）

    (define (parse-em-or-strong str start . maybe-refmap)
      (let* ((len (string-length str))
             (refmap (if (null? maybe-refmap) #f (car maybe-refmap)))
             (delims
               (let scan
                 ((pos start) (acc '()))
                 (cond ((>= pos len) (reverse acc))
                       ((and (emph-marker-at? str pos) (not (backslash-escaped-at? str pos)))
                        (let* ((run-len (marker-run-len str pos))
                               (ch (string-ref str pos))
                               (underscore? (char=? ch #\_))
                               (intraword? (and underscore? (intraword-run? str pos run-len)))
                               (left (left-flanking? str pos run-len))
                               (right (right-flanking? str pos run-len))
                               (can-open (and left (not intraword?)))
                               (can-close (and right (not intraword?)))
                               (delim (list pos run-len ch can-open can-close))
                              ) ;
                          (if (or can-open can-close)
                            (scan (+ pos run-len) (cons delim acc))
                            (scan (+ pos run-len) acc)
                          ) ;if
                        ) ;let*
                       ) ;
                       ((code-span-end str pos) (scan (code-span-end str pos) acc))
                       ((skip-html-or-autolink str pos)
                        (let ((skip (skip-html-or-autolink str pos)))
                          (scan skip acc)
                        ) ;let
                       ) ;
                       ((and (< pos len) (char=? (string-ref str pos) #\!))
                        (let ((img (and (image-start? str pos) (parse-image str pos refmap))))
                          (if
                            (and img (eq? (cmark-node-type (car img)) 'image))
                            (scan (cdr img) acc)
                            (scan (+ pos 1) acc)
                          ) ;if
                        ) ;let
                       ) ;
                       ((and (< pos len) (char=? (string-ref str pos) #\[))
                        (let ((lnk (parse-link str pos refmap)))
                          (if
                            (and lnk (eq? (cmark-node-type (car lnk)) 'link))
                            (scan (cdr lnk) acc)
                            (scan (+ pos 1) acc)
                          ) ;if
                        ) ;let
                       ) ;
                       (else (scan (+ pos 1) acc))
                 ) ;cond
               ) ;let
             ) ;delims
             (n-delims (length delims))
             (delims-v (list->vector delims))
             (lens
               (list->vector (map (lambda (d) (cadr d)) delims))
             ) ;lens
             (can-open-v
               (list->vector (map (lambda (d) (cadddr d)) delims))
             ) ;can-open-v
             (can-close-v
               (list->vector
                 (map (lambda (d) (car (cddddr d))) delims)
               ) ;list->vector
             ) ;can-close-v
             (char-v
               (list->vector (map (lambda (d) (caddr d)) delims))
             ) ;char-v
             (alive (make-vector n-delims #t))
            ) ;
        (define (same-char? oi ci)
          (char=? (vector-ref char-v oi) (vector-ref char-v ci))
        ) ;define
        (define (match-cond? oi ci)
          (let ((c-open (vector-ref can-open-v ci))
                (o-close (vector-ref can-close-v oi))
                (o-len (cadr (vector-ref delims-v oi)))
                (c-len (cadr (vector-ref delims-v ci)))
               ) ;
            (not
              (and (or c-open o-close)
                (not (= (modulo c-len 3) 0))
                (= (modulo (+ o-len c-len) 3) 0)
              ) ;and
            ) ;not
          ) ;let
        ) ;define
        (define pairs
          (let collect
            ((ci 0) (acc '()))
            (if (>= ci n-delims)
              (reverse acc)
              (if (and (vector-ref alive ci) (vector-ref can-close-v ci))
                (let find-op
                  ((oi (- ci 1)))
                  (cond
                   ((< oi 0)
                    (if (not (vector-ref can-open-v ci)) (vector-set! alive ci #f))
                    (collect (+ ci 1) acc)
                   ) ;
                   ((and (vector-ref alive oi)
                      (vector-ref can-open-v oi)
                      (same-char? oi ci)
                      (match-cond? oi ci)
                    ) ;and
                    (let* ((oi-len (vector-ref lens oi))
                           (ci-len (vector-ref lens ci))
                           (use (if (and (>= oi-len 2) (>= ci-len 2)) 2 1))
                          ) ;
                      (vector-set! lens oi (- oi-len use))
                      (vector-set! lens ci (- ci-len use))
                      (let kill
                        ((k (+ oi 1)))
                        (if (< k ci) (begin (vector-set! alive k #f) (kill (+ k 1))) '())
                      ) ;let
                      (if (= (vector-ref lens ci) 0) (vector-set! alive ci #f))
                      (if (= (vector-ref lens oi) 0) (vector-set! alive oi #f))
                      (if (vector-ref alive ci)
                        (collect ci (cons (list oi ci use) acc))
                        (collect (+ ci 1) (cons (list oi ci use) acc))
                      ) ;if
                    ) ;let*
                   ) ;
                   (else (find-op (- oi 1)))
                  ) ;cond
                ) ;let
                (collect (+ ci 1) acc)
              ) ;if
            ) ;if
          ) ;let
        ) ;define
        (define (initial-segs lo hi)
          (let loop
            ((di 0) (cur lo) (acc '()))
            (if (>= di n-delims)
              (reverse
                (if (< cur hi) (cons (list cur hi (substring str cur hi)) acc) acc)
              ) ;reverse
              (let* ((d (vector-ref delims-v di))
                     (d-pos (car d))
                     (d-len (cadr d))
                     (d-end (+ d-pos d-len))
                    ) ;
                (if (< d-pos cur)
                  (loop (+ di 1) cur acc)
                  (loop (+ di 1)
                    d-end
                    (cons (list d-pos d-end (substring str d-pos d-end))
                      (if (> d-pos cur) (cons (list cur d-pos (substring str cur d-pos)) acc) acc)
                    ) ;cons
                  ) ;loop
                ) ;if
              ) ;let*
            ) ;if
          ) ;let
        ) ;define
        (define (make-emph-or-strong segs use)
          (let ((node (if (= use 2) (make-cmark-strong) (make-cmark-emph))))
            (let add
              ((s segs) (buf "") (children '()))
              (cond
               ((null? s)
                (for-each (lambda (c) (cmark-node-append-child! node c))
                  (reverse
                    (if (string=? buf "")
                      children
                      (fold (lambda (n acc) (append-inline-node acc n))
                        children
                        (parse-inlines buf refmap)
                      ) ;fold
                    ) ;if
                  ) ;reverse
                ) ;for-each
               ) ;
               ((string? (caddr (car s)))
                (add (cdr s) (string-append buf (caddr (car s))) children)
               ) ;
               (else
                 (let* ((parsed (parse-inlines buf refmap))
                        (append-fn (lambda (n a) (append-inline-node a n)))
                        (acc2 (fold append-fn children parsed))
                       ) ;
                   (add (cdr s) "" (cons (caddr (car s)) acc2))
                 ) ;let*
               ) ;else
              ) ;cond
            ) ;let
            node
          ) ;let
        ) ;define
        (define (apply-pair segs used-o used-c pair)
          (let* ((oi (car pair))
                 (ci (cadr pair))
                 (use (caddr pair))
                 (op-rec (vector-ref delims-v oi))
                 (cl-rec (vector-ref delims-v ci))
                 (op-pos (car op-rec))
                 (op-len0 (cadr op-rec))
                 (cl-pos (car cl-rec))
                 (cl-len0 (cadr cl-rec))
                 (op-end (+ op-pos op-len0))
                 (op-used (+ (vector-ref used-o oi) use))
                 (cl-used (+ (vector-ref used-c ci) use))
                 (op-rem (- op-len0 op-used))
                 (cl-rem (- cl-len0 cl-used))
                ) ;
            (vector-set! used-o oi op-used)
            (vector-set! used-c ci cl-used)
            ;; 增量处理：跳过完全在 op-pos 之前的 segs（共享引用，不拷贝），
            ;; walk 只处理 op..cl 区间，cl 之后的 segs 共享拼接
            ;; 注意：skip 每对需从头重扫此前已处理的前缀 segs，相邻 pairs 时前缀最长，
            ;; 总开销 O(p²)（常数小，实测 unclosed 1600 约 72ms；进一步优化见 1028 5.3）
            (let* ((skip-info
                     (let skip
                       ((s0 segs) (before '()))
                       (if
                         (or (null? s0) (> (cadr (car s0)) op-pos))
                         ;; before 为反向累积（配合 walk 最终 reverse）
                         (cons s0 before)
                         (skip (cdr s0) (cons (car s0) before))
                       ) ;if
                     ) ;let
                   ) ;skip-info
                   (walk-segs (car skip-info))
                   (before-rev (cdr skip-info))
                  ) ;
              (let walk
                ((s walk-segs) (acc-rev before-rev) (children '()) (node-placed? #f))
                (cond
                 ((null? s)
                  (reverse
                    (if node-placed?
                      acc-rev
                      (let ((emph-node (make-emph-or-strong (reverse children) use)))
                        (cons (list op-end cl-pos emph-node) acc-rev)
                      ) ;let
                    ) ;if
                  ) ;reverse
                 ) ;
                 ((and (<= (car (car s)) op-pos) (< op-pos (cadr (car s))))
                  (walk (cdr s)
                    (if (> op-rem 0)
                      (let ((op-text (substring str op-pos (+ op-pos op-rem))))
                        (cons (list op-pos (+ op-pos op-rem) op-text) acc-rev)
                      ) ;let
                      acc-rev
                    ) ;if
                    children
                    node-placed?
                  ) ;walk
                 ) ;
                 ((and (<= (car (car s)) cl-pos) (< cl-pos (cadr (car s))))
                  (walk (cdr s)
                    (let ((acc1
                            (if node-placed?
                              acc-rev
                              (let* ((rev-children (reverse children))
                                     (emph-node (make-emph-or-strong rev-children use))
                                    ) ;
                                (cons (list op-end cl-pos emph-node) acc-rev)
                              ) ;let*
                            ) ;if
                          ) ;acc1
                         ) ;
                      (if (> cl-rem 0)
                        (let ((cl-text (substring str cl-pos (+ cl-pos cl-rem))))
                          (cons (list cl-pos (+ cl-pos cl-rem) cl-text) acc1)
                        ) ;let
                        acc1
                      ) ;if
                    ) ;let
                    children
                    #t
                  ) ;walk
                 ) ;
                 ((and (>= (car (car s)) op-end) (<= (cadr (car s)) cl-pos))
                  (walk (cdr s) acc-rev (cons (car s) children) node-placed?)
                 ) ;
                 ((> (car (car s)) cl-pos)
                  ;; 越过 cl-pos：剩余 segs 与当前 pair 无关，共享保留
                  (append (reverse acc-rev) s)
                 ) ;
                 (else (walk (cdr s) (cons (car s) acc-rev) children node-placed?))
                ) ;cond
              ) ;let
            ) ;let*
          ) ;let*
        ) ;define
        ;; 1033：区间树递归折叠。将 relevant pairs（cl-ascending 平坦列表）组织成区间森林，
        ;; 按「内层先」递归后序折叠，使每个 apply-pair 只处理自己的 [op,cl] 子区间，
        ;; 消除原扁平 fold 中每对从头重扫已折叠前缀的 O(p²) skip 开销。
        (define (pair-op p)
          (car (vector-ref delims-v (car p)))
        ) ;define
        (define (pair-cl p)
          (car (vector-ref delims-v (cadr p)))
        ) ;define
        ;; 通用辅助 prepend-rev / stable-sort 已提取到 (liii cmark text)。
        (define (split-segs-at segs pos)
          ;; 返回 (cons before from)，before=end<=pos 的 segs（顺序），from=首个 end>pos 起的列表
          (let loop
            ((s segs) (before '()))
            (if
              (or (null? s) (> (cadr (car s)) pos))
              (cons (reverse before) s)
              (loop (cdr s) (cons (car s) before))
            ) ;if
          ) ;let
        ) ;define
        ;; 节点 = (op cl children pair-list)。children 为严格嵌套的 direct 子（op 递增），
        ;; pair-list 为同一 (op,cl) 区间被多次消费的 pair 序列（按 original 顺序，通常 1 个）。
        (define (node-op n)
          (car n)
        ) ;define
        (define (node-cl n)
          (cadr n)
        ) ;define
        (define (node-children n)
          (caddr n)
        ) ;define
        (define (node-pairs n)
          (cadddr n)
        ) ;define
        (define (add-child! n c)
          (set-car! (cddr n) (cons c (caddr n)))
        ) ;define
        (define (add-pair! n p)
          (set-car! (cdddr n) (cons p (cadddr n)))
        ) ;define
        (define (make-node op cl p)
          (list op cl '() (list p))
        ) ;define
        ;; 强调区间树排序键：按 op 升序，同 op 再按 cl 降序（父区间在前），再按原索引；
        ;; 传给 (liii cmark text) 的 stable-sort 做稳定排序（不用 srfi-132 list-sort：
        ;; goldfish 该实现为弱排序，乱序输入会退化 O(n²)）。
        (define (tag< a b)
          (let ((oa (pair-op (car a)))
                (ob (pair-op (car b)))
                (ca (pair-cl (car a)))
                (cb (pair-cl (car b)))
               ) ;
            (cond ((< oa ob) #t)
                  ((> oa ob) #f)
                  ((> ca cb) #t)
                  ((< ca cb) #f)
                  (else (< (cdr a) (cdr b)))
            ) ;cond
          ) ;let
        ) ;define
        (define (finalize-node n)
          (list (car n) (cadr n) (reverse (caddr n)) (reverse (cadddr n)))
        ) ;define
        (define (build-forest pl)
          ;; 按 op 排序（tag<：见上）后，用栈建区间树；intervals 两两嵌套或不相交。
          ;; 栈顶 cl < 当前 op 时该区间闭合（不相交根）；同 (op,cl) 多次消费的对手链入同一节点。
          (let* ((tagged
                   (let tag
                     ((ps pl) (idx 0) (acc '()))
                     (if (null? ps)
                       (reverse acc)
                       (tag (cdr ps) (+ idx 1) (cons (cons (car ps) idx) acc))
                     ) ;if
                   ) ;let
                 ) ;tagged
                 (op-asc (stable-sort tagged tag<))
                ) ;
            (let loop
              ((ps op-asc) (stack '()) (roots-rev '()))
              (if (null? ps)
                (let drain
                  ((st stack) (acc-rev roots-rev))
                  (if (null? st)
                    (reverse acc-rev)
                    (drain (cdr st) (cons (finalize-node (car st)) acc-rev))
                  ) ;if
                ) ;let
                (let* ((p (car (car ps)))
                       (op (pair-op p))
                       (cl (pair-cl p))
                       (topo
                         (let pop
                           ((st stack) (acc-rev roots-rev))
                           (if
                             (or (null? st) (>= (node-cl (car st)) op))
                             (cons st acc-rev)
                             (pop (cdr st) (cons (finalize-node (car st)) acc-rev))
                           ) ;if
                         ) ;let
                       ) ;topo
                       (stack2 (car topo))
                       (roots1 (cdr topo))
                      ) ;
                  (cond
                   ((null? stack2) (loop (cdr ps) (cons (make-node op cl p) stack2) roots1))
                   ((and (= (node-op (car stack2)) op) (= (node-cl (car stack2)) cl))
                    ;; 同 (op,cl) 多次消费：链到栈顶（按其原序）
                    (add-pair! (car stack2) p)
                    (loop (cdr ps) stack2 roots1)
                   ) ;
                   (else
                     ;; 嵌套子区间
                     (add-child! (car stack2) (make-node op cl p))
                     (loop (cdr ps) stack2 roots1)
                   ) ;else
                  ) ;cond
                ) ;let*
              ) ;if
            ) ;let
          ) ;let*
        ) ;define
        (define (fold-forest roots segs used-o used-c)
          ;; 后序折叠：尾递归累积反向 acc-rev，用 prepend-rev（(liii cmark text)）逐元素 cons，
          ;; 避免对增长中的结果反复 append（goldfish append 深拷贝全部参数 → O(n²)）。
          (let loop
            ((rs roots) (segs segs) (acc-rev '()))
            (if (null? rs)
              (reverse (prepend-rev segs acc-rev))
              (let* ((r0 (car rs))
                     (op (node-op r0))
                     (cl (node-cl r0))
                     (children (node-children r0))
                     (pairs (node-pairs r0))
                     (front+tail (split-segs-at segs op))
                     (front (car front+tail))
                     (tail (cdr front+tail))
                     (inner (if (null? children) tail (fold-forest children tail used-o used-c)))
                     (folded
                       (let fold-pairs
                         ((pl pairs) (cur inner))
                         (if (null? pl)
                           cur
                           (fold-pairs (cdr pl) (apply-pair cur used-o used-c (car pl)))
                         ) ;if
                       ) ;let
                     ) ;folded
                     (part+rest (split-segs-at folded cl))
                     (part (car part+rest))
                     (rest (cdr part+rest))
                     (acc2 (prepend-rev part (prepend-rev front acc-rev)))
                    ) ;
                (loop (cdr rs) rest acc2)
              ) ;let*
            ) ;if
          ) ;let
        ) ;define
        (define (render-segs segs)
          (let loop
            ((s segs) (acc '()) (buf ""))
            (cond
             ((null? s)
              (reverse
                (if (string=? buf "")
                  acc
                  (let* ((inner (parse-inlines buf refmap))
                         (append-fn (lambda (n a) (append-inline-node a n)))
                         (folded (fold append-fn acc inner))
                        ) ;
                    folded
                  ) ;let*
                ) ;if
              ) ;reverse
             ) ;
             ((string? (caddr (car s)))
              (loop (cdr s) acc (string-append buf (caddr (car s))))
             ) ;
             (else
               (let* ((parsed (parse-inlines buf refmap))
                      (acc2 (fold (lambda (n a) (append-inline-node a n)) acc parsed))
                     ) ;
                 (loop (cdr s) (cons (caddr (car s)) acc2) "")
               ) ;let*
             ) ;else
            ) ;cond
          ) ;let
        ) ;define
        (define (build lo hi)
          (let* ((used-o (make-vector n-delims 0))
                 (used-c (make-vector n-delims 0))
                 (relevant
                   (let keep
                     ((pl pairs) (acc '()))
                     (cond ((null? pl) (reverse acc))
                           ((let* ((p (car pl))
                                   (op-pos (car (vector-ref delims-v (car p))))
                                   (cl-pos (car (vector-ref delims-v (cadr p))))
                                  ) ;
                              (and (>= op-pos lo) (<= cl-pos hi))
                            ) ;let*
                            (keep (cdr pl) (cons (car pl) acc))
                           ) ;
                           (else (keep (cdr pl) acc))
                     ) ;cond
                   ) ;let
                 ) ;relevant
                 (final-segs (fold-forest (build-forest relevant) (initial-segs lo hi) used-o used-c)
                 ) ;final-segs
                ) ;
            (if
              (or (= n-delims 0)
                (not (= (car (vector-ref delims-v 0)) start))
                (and (= (vector-ref used-o 0) 0) (= (vector-ref used-c 0) 0))
              ) ;or
              (cons
                (list (make-cmark-text (substring str start (+ start 1))))
                (+ start 1)
              ) ;cons
              (cons (render-segs final-segs) hi)
            ) ;if
          ) ;let*
        ) ;define
        (build start len)
      ) ;let*
    ) ;define
    (define (parse-emph str start . maybe-refmap)
      (let* ((refmap (if (null? maybe-refmap) #f (car maybe-refmap)))
             (result (parse-em-or-strong str start refmap))
            ) ;
        (cons (car (car result)) (cdr result))
      ) ;let*
    ) ;define
    (define (parse-strong str start . maybe-refmap)
      (let* ((refmap (if (null? maybe-refmap) #f (car maybe-refmap)))
             (result (parse-em-or-strong str start refmap))
            ) ;
        (cons (car (car result)) (cdr result))
      ) ;let*
    ) ;define
    (define (parse-strikeout str start . maybe-refmap)
      (let* ((len (string-length str))
             (refmap (if (null? maybe-refmap) #f (car maybe-refmap)))
             (end-pos (find-matching-strikeout str (+ start 2)))
            ) ;
        (if end-pos
          (let ((node (make-cmark-strikeout))
                (children (parse-inlines (substring str (+ start 2) end-pos) refmap))
               ) ;
            (for-each (lambda (child) (cmark-node-append-child! node child)) children)
            (cons node (+ end-pos 2))
          ) ;let
          (cons (make-cmark-text "~~") (+ start 2))
        ) ;if
      ) ;let*
    ) ;define
    (define (link-start? str pos)
      (and (< pos (string-length str)) (char=? (string-ref str pos) #\[))
    ) ;define
    (define (resolve-reference-target refmap label)
      (let ((entry (reference-map-resolve refmap label)))
        (and entry (cdr entry))
      ) ;let
    ) ;define
    ;; 引用链接节点 + 后续位置（§3.14）
    (define (link-node-cons text target refmap end)
      (cons (make-link-node-from-reference text target refmap) (+ end 1))
    ) ;define

    ;; 引用图片节点 + 后续位置（§3.14）
    (define (image-node-cons alt-text target refmap end)
      (cons (make-image-node-from-reference alt-text target refmap) (+ end 1))
    ) ;define

    (define (make-link-node-from-reference text-content reference-target refmap)
      (let ((link-node (make-cmark-link (car reference-target) (cdr reference-target))))
        (for-each (lambda (node) (cmark-node-append-child! link-node node))
          (parse-inlines text-content refmap)
        ) ;for-each
        link-node
      ) ;let
    ) ;define
    (define (make-image-node-from-reference alt-text reference-target refmap)
      (let ((image-node (make-cmark-image (car reference-target) (cdr reference-target)))
            (alt-nodes (parse-inlines alt-text refmap))
           ) ;
        (for-each (lambda (node) (cmark-node-append-child! image-node node))
          (if (null? alt-nodes) (list (make-cmark-text "")) alt-nodes)
        ) ;for-each
        image-node
      ) ;let
    ) ;define
    (define (contains-link-node? nodes)
      (if (null? nodes)
        #f
        (or (eq? (cmark-node-type (car nodes)) 'link)
          (contains-link-node? (cmark-node-children (car nodes)))
          (contains-link-node? (cdr nodes))
        ) ;or
      ) ;if
    ) ;define
    (define (make-link-node-with-nodes url title text-nodes)
      (let ((link-node (make-cmark-link url title)))
        (for-each (lambda (node) (cmark-node-append-child! link-node node)) text-nodes)
        link-node
      ) ;let
    ) ;define
    (define (parse-link str start . maybe-refmap)
      (let* ((len (string-length str))
             (refmap (if (null? maybe-refmap) #f (car maybe-refmap)))
             (bracket-end (find-matching-bracket str start))
            ) ;
        (cond ((not bracket-end) (cons (make-cmark-text "[") (+ start 1)))
              ((and (< (+ bracket-end 1) len) (char=? (string-ref str (+ bracket-end 1)) #\())
               (let* ((text-content (substring str (+ start 1) bracket-end))
                      (text-nodes (parse-inlines text-content refmap))
                     ) ;
                 (if (contains-link-node? text-nodes)
                   (cons (make-cmark-text "[") (+ start 1))
                   (let ((result (parse-inline-link-rest str (+ bracket-end 2))))
                     (if result
                       (let* ((link-target (car result))
                              (link-url (car link-target))
                              (link-title (cdr link-target))
                              (link-node (make-link-node-with-nodes link-url link-title text-nodes))
                             ) ;
                         (cons link-node (cdr result))
                       ) ;let*
                       (let ((target (resolve-reference-target refmap text-content)))
                         (if target
                           (link-node-cons text-content target refmap bracket-end)
                           (cons (make-cmark-text "[") (+ start 1))
                         ) ;if
                       ) ;let
                     ) ;if
                   ) ;let
                 ) ;if
               ) ;let*
              ) ;
              ((and (< (+ bracket-end 1) len) (char=? (string-ref str (+ bracket-end 1)) #\[))
               (let* ((text-content (substring str (+ start 1) bracket-end))
                      (text-nodes (parse-inlines text-content refmap))
                      (ref-start (+ bracket-end 2))
                     ) ;
                 (if (contains-link-node? text-nodes)
                   (cons (make-cmark-text "[") (+ start 1))
                   (cond
                    ((and (< ref-start len) (char=? (string-ref str ref-start) #\]))
                     (let ((target (resolve-reference-target refmap text-content)))
                       (if target
                         (cons (make-link-node-from-reference text-content target refmap)
                           (+ ref-start 1)
                         ) ;cons
                         (cons (make-cmark-text "[") (+ start 1))
                       ) ;if
                     ) ;let
                    ) ;
                    (else
                      (let ((ref-end (find-matching-bracket str (+ bracket-end 1))))
                        (if ref-end
                          (let* ((label (substring str (+ bracket-end 2) ref-end))
                                 (target (resolve-reference-target refmap label))
                                ) ;
                            (if target
                              (link-node-cons text-content target refmap ref-end)
                              (cons (make-cmark-text "[") (+ start 1))
                            ) ;if
                          ) ;let*
                          (cons (make-cmark-text "[") (+ start 1))
                        ) ;if
                      ) ;let
                    ) ;else
                   ) ;cond
                 ) ;if
               ) ;let*
              ) ;
              (else
                (let* ((text-content (substring str (+ start 1) bracket-end))
                       (text-nodes (parse-inlines text-content refmap))
                      ) ;
                  (if (contains-link-node? text-nodes)
                    (cons (make-cmark-text "[") (+ start 1))
                    (let ((target (resolve-reference-target refmap text-content)))
                      (if target
                        (cons (make-link-node-from-reference text-content target refmap)
                          (+ bracket-end 1)
                        ) ;cons
                        (cons (make-cmark-text "[") (+ start 1))
                      ) ;if
                    ) ;let
                  ) ;if
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define
    (define (image-start? str pos)
      (and (< (+ pos 1) (string-length str))
        (char=? (string-ref str pos) #\!)
        (char=? (string-ref str (+ pos 1)) #\[)
      ) ;and
    ) ;define
    (define (parse-image str start . maybe-refmap)
      (let* ((len (string-length str))
             (refmap (if (null? maybe-refmap) #f (car maybe-refmap)))
             (bracket-start (+ start 1))
             (bracket-end (find-matching-bracket str bracket-start))
            ) ;
        (cond ((not bracket-end) (cons (make-cmark-text "!") (+ start 1)))
              ((and (< (+ bracket-end 1) len) (char=? (string-ref str (+ bracket-end 1)) #\())
               (let* ((alt-text (substring str (+ start 2) bracket-end))
                      (result (parse-inline-link-rest str (+ bracket-end 2)))
                     ) ;
                 (if result
                   (let* ((url (car (car result)))
                          (title (cdr (car result)))
                          (image-node (make-cmark-image url title))
                          (alt-nodes (parse-inlines alt-text refmap))
                         ) ;
                     (for-each (lambda (node) (cmark-node-append-child! image-node node))
                       (if (null? alt-nodes) (list (make-cmark-text "")) alt-nodes)
                     ) ;for-each
                     (cons image-node (cdr result))
                   ) ;let*
                   (cons (make-cmark-text "!") (+ start 1))
                 ) ;if
               ) ;let*
              ) ;
              ((and (< (+ bracket-end 1) len) (char=? (string-ref str (+ bracket-end 1)) #\[))
               (let* ((alt-text (substring str (+ start 2) bracket-end))
                      (ref-start (+ bracket-end 2))
                     ) ;
                 (cond
                  ((and (< ref-start len) (char=? (string-ref str ref-start) #\]))
                   (let ((target (resolve-reference-target refmap alt-text)))
                     (if target
                       (image-node-cons alt-text target refmap ref-start)
                       (cons (make-cmark-text "!") (+ start 1))
                     ) ;if
                   ) ;let
                  ) ;
                  (else
                    (let ((ref-end (find-matching-bracket str (+ bracket-end 1))))
                      (if ref-end
                        (let* ((label (substring str (+ bracket-end 2) ref-end))
                               (target (resolve-reference-target refmap label))
                              ) ;
                          (if target
                            (image-node-cons alt-text target refmap ref-end)
                            (cons (make-cmark-text "!") (+ start 1))
                          ) ;if
                        ) ;let*
                        (cons (make-cmark-text "!") (+ start 1))
                      ) ;if
                    ) ;let
                  ) ;else
                 ) ;cond
               ) ;let*
              ) ;
              (else
                (let* ((alt-text (substring str (+ start 2) bracket-end))
                       (target (resolve-reference-target refmap alt-text))
                      ) ;
                  (if target
                    (image-node-cons alt-text target refmap bracket-end)
                    (cons (make-cmark-text (substring str start (+ bracket-end 1)))
                      (+ bracket-end 1)
                    ) ;cons
                  ) ;if
                ) ;let*
              ) ;else
        ) ;cond
      ) ;let*
    ) ;define
    (define (parse-inline-html str start)
      (let* ((len (string-length str)) (end-pos (find-inline-html-end str start)))
        (if end-pos
          (let* ((html-content (substring str start end-pos))
                 (html-node (make-cmark-html-inline html-content))
                ) ;
            (cons html-node end-pos)
          ) ;let*
          (cons (make-cmark-text "<") (+ start 1))
        ) ;if
      ) ;let*
    ) ;define
    (define (linebreak? str pos)
      (or (backslash-hard-linebreak? str pos)
        (and (< pos (string-length str)) (char=? (string-ref str pos) #\newline))
      ) ;or
    ) ;define
    (define (parse-linebreak str start)
      (cond ((backslash-hard-linebreak? str start)
             (cons (make-cmark-linebreak) (+ start 2))
            ) ;
            ((and (< start (string-length str)) (char=? (string-ref str start) #\newline))
             (if (hard-linebreak-by-spaces? str start)
               (cons (make-cmark-linebreak) (+ start 1))
               (cons (make-cmark-softbreak) (+ start 1))
             ) ;if
            ) ;
            (else (error 'parse-linebreak "linebreak expected at position" start))
      ) ;cond
    ) ;define
    ;; str 位置 pos 是 `*` 或 `_`（强调标记候选）
    (define (emph-marker-at? str pos)
      (or (char=? (string-ref str pos) #\*) (char=? (string-ref str pos) #\_))
    ) ;define

    (define (parse-inlines str . maybe-refmap)
      (let ((len (string-length str)))
        (let ((refmap (if (null? maybe-refmap) #f (car maybe-refmap))))
          (let loop
            ((pos 0) (nodes '()))
            (cond ((>= pos len) (reverse nodes))
                  ((inline-code-start? str pos)
                   (let ((result (parse-inline-code str pos)))
                     (loop (cdr result) (append-inline-node nodes (car result)))
                   ) ;let
                  ) ;
                  ((or (strong-start? str pos) (emph-start? str pos))
                   (let ((result (parse-em-or-strong str pos refmap)))
                     (loop (cdr result)
                       (let append-nodes
                         ((remaining (car result)) (acc nodes))
                         (if (null? remaining)
                           acc
                           (append-nodes (cdr remaining) (append-inline-node acc (car remaining)))
                         ) ;if
                       ) ;let
                     ) ;loop
                   ) ;let
                  ) ;
                  ((strikeout-start? str pos)
                   (let ((result (parse-strikeout str pos refmap)))
                     (loop (cdr result) (append-inline-node nodes (car result)))
                   ) ;let
                  ) ;
                  ((image-start? str pos)
                   (let ((result (parse-image str pos refmap)))
                     (loop (cdr result) (append-inline-node nodes (car result)))
                   ) ;let
                  ) ;
                  ((link-start? str pos)
                   (let ((result (parse-link str pos refmap)))
                     (loop (cdr result) (append-inline-node nodes (car result)))
                   ) ;let
                  ) ;
                  ((emoji-start? str pos)
                   (let ((result (parse-emoji str pos)))
                     (loop (cdr result) (append-inline-node nodes (car result)))
                   ) ;let
                  ) ;
                  ((or (autolink-start? str pos) (inline-html-start? str pos))
                   (let* ((a-result (and (autolink-start? str pos) (parse-autolink str pos)))
                          (result (or a-result (parse-inline-html str pos)))
                         ) ;
                     (loop (cdr result) (append-inline-node nodes (car result)))
                   ) ;let*
                  ) ;
                  ((and (bare-autolink-enabled?) (bare-autolink-start? str pos))
                   (let ((result (parse-bare-autolink str pos)))
                     (loop (cdr result) (append-inline-node nodes (car result)))
                   ) ;let
                  ) ;
                  ((and (< pos len) (char=? (string-ref str pos) #\<))
                   (loop (+ pos 1) (append-inline-node nodes (make-cmark-text "<")))
                  ) ;
                  ((and (< pos len) (char=? (string-ref str pos) #\~))
                   (loop (+ pos 1) (append-inline-node nodes (make-cmark-text "~")))
                  ) ;
                  ((and (< pos len) (char=? (string-ref str pos) #\:))
                   (loop (+ pos 1) (append-inline-node nodes (make-cmark-text ":")))
                  ) ;
                  ((backslash-hard-linebreak? str pos)
                   (loop (+ pos 2)
                     (append-inline-node (trim-trailing-spaces-from-last-text-node nodes)
                       (make-cmark-linebreak)
                     ) ;append-inline-node
                   ) ;loop
                  ) ;
                  ;; \( 是 math 定界（数学扩展），须在 backslash-char? 转义分支之前；
                  ;; \\( 由转义分支先消费 \\，主循环不会停在被转义的 \( 上
                  ((math-inline-start? str pos)
                   (let ((result (parse-math-inline str pos)))
                     ;; $$ 行内公式两侧文本剥除紧邻空格（Math30：a $$x$$ b → "a"/x/"b"）；
                     ;; 单 $ 定界保留原空格（公式是 $x+y$ 。）
                     (if
                       (and (< (+ pos 1) len)
                         (char=? (string-ref str (+ pos 1)) #\$)
                         (eq? (cmark-node-type (car result)) 'math_inline)
                       ) ;and
                       (let ((next-pos (cdr result))
                             (trimmed-nodes (trim-trailing-spaces-from-last-text-node nodes))
                            ) ;
                         (if (and (< next-pos len) (char-whitespace? (string-ref str next-pos)))
                           (loop (+ next-pos 1) (append-inline-node trimmed-nodes (car result)))
                           (loop next-pos (append-inline-node trimmed-nodes (car result)))
                         ) ;if
                       ) ;let
                       (loop (cdr result) (append-inline-node nodes (car result)))
                     ) ;if
                   ) ;let
                  ) ;
                  ((backslash-char? str pos)
                   (let ((result (parse-backslash-escape str pos)))
                     (loop (cdr result) (append-inline-node nodes (car result)))
                   ) ;let
                  ) ;
                  ((entity-start? str pos)
                   (let* ((result (parse-entity-reference str pos))
                          (decoded (node-data-ref (car result) 'content ""))
                          (updated-nodes (append-entity-text-nodes nodes decoded))
                         ) ;
                     (loop (cdr result) updated-nodes)
                   ) ;let*
                  ) ;
                  ((linebreak? str pos)
                   (let* ((result (parse-linebreak str pos))
                          (node (car result))
                          (updated-nodes
                            (if (or (eq? (cmark-node-type node) 'linebreak)
                                  (eq? (cmark-node-type node) 'softbreak)
                                ) ;or
                              (trim-trailing-spaces-from-last-text-node nodes)
                              nodes
                            ) ;if
                          ) ;updated-nodes
                         ) ;
                     (loop (cdr result) (append-inline-node updated-nodes node))
                   ) ;let*
                  ) ;
                  (else
                    (let ((next-special (find-next-special str pos)))
                      (cond
                       ((not next-special)
                        (let ((text-node (parse-text (substring str pos len))))
                          (loop len (append-inline-node nodes text-node))
                        ) ;let
                       ) ;
                       ((= next-special pos)
                        (let ((ch-text (make-cmark-text (substring str pos (+ pos 1)))))
                          (loop (+ pos 1) (append-inline-node nodes ch-text))
                        ) ;let
                       ) ;
                       (else
                         (let ((text-node (parse-inline-text str pos next-special)))
                           (loop next-special (append-inline-node nodes text-node))
                         ) ;let
                       ) ;else
                      ) ;cond
                    ) ;let
                  ) ;else
            ) ;cond
          ) ;let
        ) ;let
      ) ;let
    ) ;define
    (define (append-entity-text-nodes nodes decoded)
      (let ((len (string-length decoded)))
        (let loop
          ((pos 0) (start 0) (acc nodes))
          (cond
           ((>= pos len)
            (if (= pos start)
              acc
              (append-inline-node acc (make-cmark-text (substring decoded start pos)))
            ) ;if
           ) ;
           ((char=? (string-ref decoded pos) #\newline)
            (let* ((text-node (make-cmark-text (substring decoded start pos)))
                   (acc2 (if (= pos start) acc (append-inline-node acc text-node)))
                  ) ;
              (loop (+ pos 1) (+ pos 1) (append-inline-node acc2 (make-cmark-softbreak)))
            ) ;let*
           ) ;
           (else (loop (+ pos 1) start acc))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
