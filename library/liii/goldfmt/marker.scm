;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
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

;; (liii goldfmt marker) - goldfmt 内部标记符号
;;
;; 内部模块，不对外发布：仅供 (liii goldfmt ...) 同库兄弟模块导入。
;;
;; goldfmt 管线把注释、空行、raw string、字符字面量改写成带标记符号的普通
;; 列表（中间文本），再由 read 读回、scan 识别。历史上标记符号是固定的
;; *comment* / *newline* / *raw-string* / *char-literal*，用户源码中同样形态
;; 的引用数据会被误当成内部标记改写（issue 1468）。
;;
;; 本模块在库加载时生成一次性 nonce，拼进标记符号：*goldfmt-comment-<nonce>-*
;; 等。nonce 只存在于当前进程内存（中间文本从不落盘），用户源码在结构上不可
;; 能包含这些符号，因此用户数据永远不会与内部标记冲突。

(define-library (liii goldfmt marker)
  (export %nonce %comment-marker-symbol %newline-marker-symbol
    %raw-string-marker-symbol %char-literal-marker-symbol
    %comment-marker-tag-name %newline-marker-tag-name
    %raw-string-marker-tag-name %char-literal-marker-tag-name
  ) ;export
  (import (scheme base) (scheme time) (srfi srfi-13))
  (begin

    ;; nonce = <current-jiffy 的十六进制> + "-" + <gensym 自增序号>。
    ;; jiffy 保证不同进程互不相同，gensym 序号保证同进程内重复加载
    ;; （如 REPL 里重新 import）也不重叠；两者拼接仅含 [0-9a-f-]，
    ;; 读回后为合法普通符号，不会与用户代码冲突。
    (define %nonce
      (let* ((g (symbol->string (gensym "")))
             (dash (string-index-right g #\-))
             (serial (if dash (substring g (+ dash 1)) "0"))
            ) ;
        (string-append (number->string (current-jiffy) 16) "-" serial)
      ) ;let*
    ) ;define

    (define (%make-marker-symbol stem)
      (string->symbol (string-append "*goldfmt-" stem "-" %nonce "-*"))
    ) ;define

    (define (%make-marker-tag-name stem)
      (symbol->string (%make-marker-symbol stem))
    ) ;define

    (define %comment-marker-symbol (%make-marker-symbol "comment"))
    (define %newline-marker-symbol (%make-marker-symbol "newline"))
    (define %raw-string-marker-symbol (%make-marker-symbol "raw-string"))
    (define %char-literal-marker-symbol (%make-marker-symbol "char-literal"))

    (define %comment-marker-tag-name (symbol->string %comment-marker-symbol))
    (define %newline-marker-tag-name (symbol->string %newline-marker-symbol))
    (define %raw-string-marker-tag-name (symbol->string %raw-string-marker-symbol))
    (define %char-literal-marker-tag-name
      (symbol->string %char-literal-marker-symbol)
    ) ;define
  ) ;begin
) ;define-library
