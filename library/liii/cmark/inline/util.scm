;;
;; Copyright (C) 2026 Liii Network
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;; http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

(define-library (liii cmark inline util)
  (import (scheme base))
  (import (liii cmark node))
  (import (liii cmark text))
  (import (liii cmark common))
  (import (liii cmark inline link))
  (import (liii cmark inline entity))
  (import (liii cmark inline autolink))
  (import (liii cmark inline emoji))
  (import (liii cmark inline escape))
  (export char-run-length)
  (export find-matching-backticks)
  (export string-find-char)
  (export find-matching-bracket)
  (export code-span-end)
  (export parse-link-destination)
  (export parse-inline-link-rest)
  (export skip-html-or-autolink)
  (export entity-start?)
  (export parse-entity-reference)
  (export decode-entity-references-in-string)
  (export backslash-char?)
  (export backslash-hard-linebreak?)
  (export hard-linebreak-by-spaces?)
  (export parse-backslash-escape)
  (export node-data-ref)
  (export append-inline-node)
  (export trim-trailing-spaces)
  (export trim-trailing-spaces-from-last-text-node)
  (export strikeout-start?)
  (export find-matching-strikeout)
  (export find-next-special)
  (begin
    (define (find-matching-backticks str start marker-len)
      (let ((len (string-length str)))
        (let loop
          ((pos start))
          (cond ((>= pos len) #f)
                ((char=? (string-ref str pos) #\`)
                 (let ((run-len (char-run-length str pos #\`)))
                   (if (= run-len marker-len) pos (loop (+ pos run-len)))
                 ) ;let
                ) ;
                (else (loop (+ pos 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (append-inline-node nodes node)
      (if
        (and (pair? nodes)
          (eq? (cmark-node-type (car nodes)) 'text)
          (eq? (cmark-node-type node) 'text)
        ) ;and
        (cons
          (make-cmark-text (string-append (node-data-ref (car nodes) 'content "")
                             (node-data-ref node 'content "")
                           ) ;string-append
          ) ;make-cmark-text
          (cdr nodes)
        ) ;cons
        (cons node nodes)
      ) ;if
    ) ;define

    (define (trim-trailing-spaces str)
      (let ((len (string-length str)))
        (let loop
          ((end len))
          (if
            (and (> end 0) (char=? (string-ref str (- end 1)) #\space))
            (loop (- end 1))
            (substring str 0 end)
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    (define (trim-trailing-spaces-from-last-text-node nodes)
      (if
        (and (pair? nodes) (eq? (cmark-node-type (car nodes)) 'text))
        (let* ((content (node-data-ref (car nodes) 'content ""))
               (trimmed (trim-trailing-spaces content))
              ) ;
          (if (string=? trimmed "")
            (cdr nodes)
            (cons (make-cmark-text trimmed) (cdr nodes))
          ) ;if
        ) ;let*
        nodes
      ) ;if
    ) ;define

    (define (strikeout-start? str pos)
      (let ((len (string-length str)))
        (and (< (+ pos 1) len)
          (char=? (string-ref str pos) #\~)
          (char=? (string-ref str (+ pos 1)) #\~)
          (or (= pos 0) (not (char=? (string-ref str (- pos 1)) #\~)))
          (or (>= (+ pos 2) len) (not (char=? (string-ref str (+ pos 2)) #\~)))
        ) ;and
      ) ;let
    ) ;define

    (define (find-matching-strikeout str start)
      (let ((len (string-length str)))
        (let loop
          ((pos start))
          (cond ((>= (+ pos 1) len) #f)
                ((and (char=? (string-ref str pos) #\~)
                   (char=? (string-ref str (+ pos 1)) #\~)
                   (or (= pos start) (not (char=? (string-ref str (- pos 1)) #\~)))
                   (or (>= (+ pos 2) len) (not (char=? (string-ref str (+ pos 2)) #\~)))
                 ) ;and
                 pos
                ) ;
                (else (loop (+ pos 1)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

    (define (fixed-special-char? ch)
      (or (char=? ch #\`)
        (char=? ch #\*)
        (char=? ch #\_)
        (char=? ch #\[)
        (char=? ch #\!)
        (char=? ch #\&)
        (char=? ch #\<)
        (char=? ch #\$)
        (char=? ch #\~)
        (char=? ch #\\)
        (char=? ch #\newline)
      ) ;or
    ) ;define

    ;; email local 部分字符集合（find-bare-email-end 判定 email-local-char?）：
    ;; ascii 字母数字 + CommonMark email local 标点（不含 '@'）
    (define (email-local-char? ch)
      (or (and (char>=? ch #\a) (char<=? ch #\z))
        (and (char>=? ch #\A) (char<=? ch #\Z))
        (and (char>=? ch #\0) (char<=? ch #\9))
        (or (char=? ch #\.)
          (char=? ch #\!)
          (char=? ch #\#)
          (char=? ch #\$)
          (char=? ch #\%)
          (char=? ch #\&)
          (char=? ch #\')
          (char=? ch #\*)
          (char=? ch #\+)
          (char=? ch #\/)
          (char=? ch #\=)
          (char=? ch #\?)
          (char=? ch #\^)
          (char=? ch #\_)
          (char=? ch #\`)
          (char=? ch #\{)
          (char=? ch #\|)
          (char=? ch #\})
          (char=? ch #\~)
          (char=? ch #\-)
        ) ;or
      ) ;or
    ) ;define

    ;; email/URL candidate 的连续字符集合（find-bare-email-end 的 continue 判定）：
    ;; email local + '@'
    (define (email-cont-char? ch)
      (or (email-local-char? ch) (char=? ch #\@))
    ) ;define

    ;; 性能优化（1032）：find-next-special 对连续 email-candidate 字符的每个位置都会调用
    ;; bare-autolink-start?（内部 find-bare-email-end 从该位置全段扫描）。email/URL 自动链接的
    ;; 必要条件是相应区间内含 `@`（构成 email）或 `h`（构成 http:// URL）：若一段连续
    ;; email-candidate 字符既不含 '@' 也不含 'h'，则区间内每个位置 bare-autolink-start? 均判
    ;; false，可整体跳过。返回下一个需要重新判断的边界位置：
    ;; - 固定特殊字符 / 非 email-candidate 字符 / 'h' 起始的 URL - 返回该位置；
    ;; - email 起始的 `@` - 返回其前连续 email-local 段起点（若在 i 之后），否则跳过一个字符。
    (define (find-next-special-else-bound str i len)
      (let loop
        ((p (+ i 1)))
        (cond ((>= p len) len)
              ((or (fixed-special-char? (string-ref str p))
                 (not (email-cont-char? (string-ref str p)))
                 (char=? (string-ref str p) #\h)
               ) ;or
               p
              ) ;
              ((char=? (string-ref str p) #\@)
               (let local
                 ((q (- p 1)))
                 (cond ((< q 0) (+ p 1))
                       ((email-local-char? (string-ref str q)) (local (- q 1)))
                       (else
                         (let ((seg-start (+ q 1)))
                           (if (> seg-start i) seg-start (+ p 1))
                         ) ;let
                       ) ;else
                 ) ;cond
               ) ;let
              ) ;
              (else (loop (+ p 1)))
        ) ;cond
      ) ;let
    ) ;define

    (define (find-next-special str pos)
      (let ((len (string-length str)))
        (let loop
          ((i pos))
          (cond ((>= i len) #f)
                ((or (char=? (string-ref str i) #\`)
                   (char=? (string-ref str i) #\*)
                   (char=? (string-ref str i) #\_)
                   (char=? (string-ref str i) #\[)
                   (char=? (string-ref str i) #\!)
                   (char=? (string-ref str i) #\&)
                   (char=? (string-ref str i) #\<)
                   (char=? (string-ref str i) #\$)
                   (bare-autolink-start? str i)
                   (emoji-candidate-start? str i)
                   (char=? (string-ref str i) #\~)
                   (char=? (string-ref str i) #\\)
                   (char=? (string-ref str i) #\newline)
                 ) ;or
                 i
                ) ;
                (else (loop (find-next-special-else-bound str i len)))
          ) ;cond
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
