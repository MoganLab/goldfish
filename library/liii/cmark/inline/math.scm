
(define-library (liii cmark inline math)
  (import (scheme base))
  (import (scheme char))
  (import (liii cmark node))
  (import (liii cmark text))
  (export math-block-start?)
  (export math-block-end?)
  (export parse-math-block)
  (export math-inline-start?)
  (export parse-math-inline)
  (begin

    ;; \[...\] 单行块：以 \[ 开头、以 \] 结尾、定界间至少一个字符
    ;; （与 utils/markdown-math-block-util.scm 中的同名谓词保持同步）
    (define (single-line-bracket-math? line)
      (let ((len (string-length line)))
        (and (> len 3)
          (string-prefix? "\\[" line)
          (char=? (string-ref line (- len 2)) #\\)
          (char=? (string-ref line (- len 1)) #\])
        ) ;and
      ) ;let
    ) ;define

    ;; $$...$$ 单行块：以 $$ 开头、以 $$ 结尾、定界间至少一个字符
    ;; （与 utils/markdown-math-block-util.scm 中的同名谓词保持同步）
    (define (single-line-dollar-math? line)
      (let ((len (string-length line)))
        (and (> len 4)
          (string-prefix? "$$" line)
          (char=? (string-ref line (- len 2)) #\$)
          (char=? (string-ref line (- len 1)) #\$)
        ) ;and
      ) ;let
    ) ;define

    ;; 块级 math 定界：$$ / \[ 起始（含单行 \[...\]、单行 $$...$$），$$ / \] 结束
    (define (math-block-start? line)
      (or (string=? line "$$")
        (string=? line "\\[")
        (single-line-bracket-math? line)
        (single-line-dollar-math? line)
      ) ;or
    ) ;define

    (define (math-block-end? line)
      (or (string=? line "$$") (string=? line "\\]"))
    ) ;define

    (define (parse-math-block lines)
      (define (fenced-math-fence line)
        (cond
         ((and (>= (string-length line) 3)
            (string-prefix? "```" line)
            (string=? (string-trim (substring line 3)) "math")
          ) ;and
          "```"
         ) ;
         ((and (>= (string-length line) 3)
            (string-prefix? "~~~" line)
            (string=? (string-trim (substring line 3)) "math")
          ) ;and
          "~~~"
         ) ;
         (else #f)
        ) ;cond
      ) ;define
      (define (fenced-math-end-line? fence line)
        (and (string-prefix? fence line)
          (string=? (string-trim (substring line (string-length fence))) "")
        ) ;and
      ) ;define
      (if (null? lines)
        (make-cmark-math-block "")
        (let* ((first-line (string-trim (car lines))) (fence (fenced-math-fence first-line)))
          (if (single-line-bracket-math? first-line)
            ;; 单行 \[...\]：定界间内容即公式
            (make-cmark-math-block
              (string-trim (substring first-line 2 (- (string-length first-line) 2)))
            ) ;make-cmark-math-block
            (if (single-line-dollar-math? first-line)
              ;; 单行 $$...$$：定界间内容即公式
              (make-cmark-math-block
                (string-trim (substring first-line 2 (- (string-length first-line) 2)))
              ) ;make-cmark-math-block
              ;; 非围栏块按起始定界选择结束定界：$$ ↔ $$、\[ ↔ \]
              (let ((close-delim (if (string=? first-line "\\[") "\\]" "$$")))
                (let loop
                  ((rest (cdr lines)) (content-lines '()))
                  (cond
                   ((null? rest)
                    (make-cmark-math-block (string-join (reverse content-lines) "\n"))
                   ) ;
                   ((and fence (fenced-math-end-line? fence (string-trim (car rest))))
                    (make-cmark-math-block (string-join (reverse content-lines) "\n"))
                   ) ;
                   ((and (not fence) (string=? (string-trim (car rest)) close-delim))
                    (make-cmark-math-block (string-join (reverse content-lines) "\n"))
                   ) ;
                   (else (loop (cdr rest) (cons (car rest) content-lines)))
                  ) ;cond
                ) ;let
              ) ;let
            ) ;if
          ) ;if
        ) ;let*
      ) ;if
    ) ;define

    ;; inline math 定界：$...$ 或 \(...\)
    ;; （\[...\] 是展示公式，仅块级识别，见 markdown-math-block-util）
    (define (math-inline-start? str pos)
      (and (< pos (string-length str))
        (let ((ch (string-ref str pos)))
          (or (char=? ch #\$)
            (and (char=? ch #\\)
              (< (+ pos 1) (string-length str))
              (char=? (string-ref str (+ pos 1)) #\()
            ) ;and
          ) ;or
        ) ;let
      ) ;and
    ) ;define

    ;; 从 start 起找第一个未转义的 \)（前一字符不是 \），找不到返回 #f
    ;; 调用方保证 start >= 2（\( 占两个字符），故 (- i 1) 安全
    (define (unescaped-close-paren-index str start)
      (let ((len (string-length str)))
        (let loop
          ((i start))
          (cond ((>= (+ i 1) len) #f)
                ((and (char=? (string-ref str i) #\\)
                   (char=? (string-ref str (+ i 1)) #\))
                   (not (char=? (string-ref str (- i 1)) #\\))
                 ) ;and
                 i
                ) ;
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    ;; 找从 start 起第一个 "$$"（连续两个 $），找不到返回 #f
    (define (double-dollar-index str start)
      (let ((len (string-length str)))
        (let loop
          ((i start))
          (cond ((>= i len) #f)
                ((and (< (+ i 1) len)
                   (char=? (string-ref str i) #\$)
                   (char=? (string-ref str (+ i 1)) #\$)
                 ) ;and
                 i
                ) ;
                (else (loop (+ i 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    ;; 单 $ 启发式：closing $ 后不能是数字、内容非空，
    ;; 用于规避 $10 and $20 之类货币假阳性（math-in-markdown wiki 附录）。
    ;; 若 closing $ 后为数字，仅当开定界符前后均非数字时才放宽（如 $^{*}17、$x^2$1；排除 2$x$3、货币假阳性）。
    ;; 定界符内侧允许空白（7.1 Math 15/16：$ x$、$x $ 有效，空格保留在内容中）。
    ;; closing $ 前为空白且其后存在更深的 $ 时，当前匹配让位给内层 span
    ;; （Math 17：外层 $...hbox{ when $ 不吞并内层 $x > 2$）。
    (define (math-inline-candidate? str open close)
      (and (> close (+ open 1))
        (or (>= (+ close 1) (string-length str))
          (not (char-numeric? (string-ref str (+ close 1))))
          (and
            (not (char-numeric? (string-ref str (+ open 1))))
            (or (= open 0) (not (char-numeric? (string-ref str (- open 1)))))
          ) ;and
        ) ;or
      ) ;and
    ) ;define

    ;; end-pos 处的 $ 作为开定界，其后存在非数字尾随的闭合 $（让位判定用）
    (define (math-inline-scan-open? str end-pos)
      (let ((len (string-length str)))
        (let loop
          ((i (+ end-pos 1)))
          (if (>= i len)
            #f
            (let ((ch (string-ref str i)))
              (cond ((char=? ch #\$) (math-inline-candidate? str end-pos i))
                    (else (loop (+ i 1)))
              ) ;cond
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (parse-math-inline str start)
      (let ((len (string-length str)))
        (if (or (>= start len) (= len 0))
          (cons (make-cmark-text "") len)
          (let ((marker (string-ref str start)))
            (cond
             ((char=? marker #\$)
              (cond
                ;; $$ ... $$ 同行：kramdown 双用语法（wiki: kramdown/Typora/Deckset 等），
                ;; 内容非空识别为行内公式；未闭合或空内容按文本消费一个 $
                ((and (< (+ start 1) len) (char=? (string-ref str (+ start 1)) #\$))
                 (let ((end-pos (double-dollar-index str (+ start 2))))
                   (if (and end-pos (> end-pos (+ start 2)))
                     (cons (make-cmark-math-inline (substring str (+ start 2) end-pos))
                       (+ end-pos 2)
                     ) ;cons
                     (cons (make-cmark-text "$") (+ start 1))
                   ) ;if
                 ) ;let
                ) ;
                ;; 单 $ ... $：启发式限制假阳性。
                ;; 闭合 $ 前为空白且其后仍有 $（可能开启更深 span）时让位——
                ;; Math 17 外层 $...hbox{ when $ 不吞内层 $x > 2$；
                ;; 无更深 $ 时接受（Math 16 的 $x $）。
                (else
                  (let find-close
                    ((end-pos (string-index str #\$ (+ start 1))))
                    (cond ((not end-pos) (cons (make-cmark-text "$") (+ start 1)))
                          ((and (> end-pos (+ start 1))
                             (char-whitespace? (string-ref str (- end-pos 1)))
                             (math-inline-scan-open? str end-pos)
                           ) ;and
                           (cons (make-cmark-text "$") (+ start 1))
                          ) ;
                          ((math-inline-candidate? str start end-pos)
                           (let* ((content (substring str (+ start 1) end-pos))
                                  (inner (make-cmark-math-inline content))
                                 ) ;
                             (cons inner (+ end-pos 1))
                           ) ;let*
                          ) ;
                          (else (cons (make-cmark-text "$") (+ start 1)))
                    ) ;cond
                  ) ;let
                ) ;else
              ) ;cond
             ) ;
             ;; \(...\)：扫描未转义的 \)；未闭合或空内容回退为字面 (（转义语义）
             ((char=? marker #\\)
              (let ((end-pos (unescaped-close-paren-index str (+ start 2))))
                (if (and end-pos (> end-pos (+ start 2)))
                  (cons (make-cmark-math-inline (substring str (+ start 2) end-pos))
                    (+ end-pos 2)
                  ) ;cons
                  (cons (make-cmark-text "(") (+ start 2))
                ) ;if
              ) ;let
             ) ;
             (else (cons (make-cmark-text (substring str start len)) len))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
