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

;; md-stem 反向渲染的无副作用通用工具：行列表拼接、统一前缀与游程计算。
;; 仅被 (liii cmark markdown ...) 内部模块使用，不对最终用户开放。
(define-library (liii cmark markdown util)
  (import (scheme base))
  (import (liii error))
  (import (liii cmark text))
  (export md-stem-error join-lines append-line-lists prefix-lines indent-lines
    longest-char-run make-string-repeat
  ) ;export
  (begin
    ;; 反向转换统一的错误形式：所有非法 md-stem 输入都报 md-stem-error，
    ;; 消息中带上出问题的节点类型，方便定位
    (define (md-stem-error node-type . message)
      (apply error (cons 'md-stem-error (cons node-type message)))
    ) ;define

    ;; 用换行符连接行列表，得到多行文本
    (define (join-lines lines)
      (string-join lines "\n")
    ) ;define

    ;; 依序拼接多组行列表（无空白行插入，空组跳过）
    (define (append-line-lists line-lists)
      (apply append line-lists)
    ) ;define

    ;; 给每个逻辑行加前缀；空行只加前缀本身，不带尾随空格
    ;; 给每个逻辑行加前缀；空行只保留无尾随空格的前缀（合法引用结构）
    (define (prefix-lines prefix lines)
      (let ((bare-prefix (string-trim-right prefix)))
        (map (lambda (line) (if (string=? line "") bare-prefix (string-append prefix line)))
          lines
        ) ;map
      ) ;let
    ) ;define

    ;; 给非空行左侧补 n 个空格；空行保持为空
    (define (indent-lines n lines)
      (let ((pad (make-string n #\space)))
        (map (lambda (line) (if (string=? line "") "" (string-append pad line))) lines)
      ) ;let
    ) ;define

    ;; 计算字符 ch 在 str 中的最长连续游程长度（用于选 code fence 定界符）
    (define (longest-char-run ch str)
      (let ((len (string-length str)))
        (let loop
          ((i 0) (current 0) (best 0))
          (if (>= i len)
            best
            (let ((is-target (char=? (string-ref str i) ch)))
              (if is-target
                (loop (+ i 1) (+ current 1) (max best (+ current 1)))
                (loop (+ i 1) 0 best)
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

    ;; 生成 count 个字符 ch 组成的字符串；count 非正整数时报类型错误
    (define (make-string-repeat ch count)
      (unless (and (integer? count) (>= count 0))
        (type-error "make-string-repeat: count should be a non-negative integer" count)
      ) ;unless
      (make-string count ch)
    ) ;define
  ) ;begin
) ;define-library
