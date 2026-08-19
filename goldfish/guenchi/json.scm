;;  MIT License
;;  Copyright guenchi (c) 2018 - 2019
;;            Da Shen (c) 2024 - 2025
;;            (Jack) Yansong Li (c) 2025
;;  Permission is hereby granted, free of charge, to any person obtaining a copy
;;  of this software and associated documentation files (the "Software"), to deal
;;  in the Software without restriction, including without limitation the rights
;;  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;;  copies of the Software, and to permit persons to whom the Software is
;;  furnished to do so, subject to the following conditions:
;;  The above copyright notice and this permission notice shall be included in all
;;  copies or substantial portions of the Software.
;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;  SOFTWARE.

(define-library (guenchi json)
  (import (liii base)
    (liii chez)
    (liii alist)
    (liii error)
    (liii list)
    (liii string)
    (liii unicode)
  ) ;import
  (export json-string-escape)
  (begin

    (define (json-string-escape str)
      (let ((out (open-output-string)))
        (write-char #\" out)
        (let* ((bv (string->utf8 str)) (len (bytevector-length bv)))
          (let loop
            ((i 0))
            (if (>= i len)
              (begin
                (write-char #\" out)
                (get-output-string out)
              ) ;begin
              (let ((next (bytevector-advance-utf8 bv i len)))
                (cond ((= next i) (loop (+ i 1)))
                      ((= next (+ i 1))
                       (let ((c (integer->char (bytevector-u8-ref bv i))))
                         (case c
                          ((#\") (display "\\\"" out))
                          ((#\\) (display "\\\\" out))
                          ((#\/) (display "\\/" out))
                          ((#\backspace) (display "\\b" out))
                          ((#\xc) (display "\\f" out))
                          ((#\newline) (display "\\n" out))
                          ((#\return) (display "\\r" out))
                          ((#\tab) (display "\\t" out))
                          (else (write-char c out))
                         ) ;case
                         (loop next)
                       ) ;let
                      ) ;
                      (else
                        ;; 多字节 UTF-8 字符，直接输出原始字节
                        (display (copy bv (make-string (- next i)) i next) out)
                        (loop next)
                      ) ;else
                ) ;cond
              ) ;let
            ) ;if
          ) ;let
        ) ;let*
      ) ;let
    ) ;define

    ;; json-set / json-drop / json-reduce 均由 C++ 实现（src/liii_json.cpp 中的
    ;; g_json_set / g_json_drop / g_json_reduce，含变参多键路径，语义覆盖历史上的
    ;; json-set / json-drop / json-reduce 及带 * 版本），本库不再保留对应 Scheme 实现。
    ;; 历史上的 json-reduce*（多 v/p 对、p 收键路径列表的独立接口）无使用者，已删除
  ) ;begin
) ;define-library
