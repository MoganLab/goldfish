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

;; md-stem 反向渲染的代码定界计算：为行内代码选择安全长度的反引号
;; 定界符与必要填充，为围栏代码块计算 fence。只做纯计算，不做拼接。
;; 仅被 (liii cmark markdown ...) 内部模块使用，不对最终用户开放。
(define-library (liii cmark markdown code)
  (import (scheme base))
  (import (liii cmark markdown util))
  (export code-span-delimiter code-span-content code-fence-length)
  (begin
    ;; 行内代码定界符：反引号游程 = max(1, 内容最长反引号游程 + 1)。
    ;; 常见内容（无反引号）得到单个 `。
    (define (code-span-delimiter literal)
      (let ((run (longest-char-run #\` literal)))
        (make-string-repeat #\` (max 1 (+ run 1)))
      ) ;let
    ) ;define

    ;; 行内代码内容填充：两种情况需要两侧各补一个空格：
    ;; 1. 内容首或尾是空格（否则会被解析器剥掉）；
    ;; 2. 内容首或尾是反引号（否则会与定界符合并成更长游程，
    ;;    导致闭合游程长度不匹配，如 X```x```X 会解析为 (code "x")）。
    (define (code-span-content literal)
      (let ((len (string-length literal)))
        (cond ((= len 0) "")
              ((or (char=? (string-ref literal 0) #\space)
                 (char=? (string-ref literal (- len 1)) #\space)
                 (char=? (string-ref literal 0) #\`)
                 (char=? (string-ref literal (- len 1)) #\`)
               ) ;or
               (string-append " " literal " ")
              ) ;
              (else literal)
        ) ;cond
      ) ;let
    ) ;define

    ;; 围栏代码块 fence 长度：max(3, 内容最长反引号游程 + 1)。
    ;; 用反引号围栏；info 字符串由调用方拼接并保证不含反引号。
    (define (code-fence-length literal)
      (let ((run (longest-char-run #\` literal)))
        (max 3 (+ run 1))
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
