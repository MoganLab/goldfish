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

;; md-stem 反向渲染的文档层：组织顶层块间距并连接行列表。
;; 块间空行在本层统一处理，块渲染器不再自行插入。
;; 仅被 (liii cmark markdown) 入口使用，不对最终用户开放。
(define-library (liii cmark markdown document)
  (import (scheme base))
  (import (liii cmark markdown util))
  (import (liii cmark markdown validate))
  (import (liii cmark markdown blocks))
  (export render-md-stem-document)
  (begin
    ;; 已校验的文档体（(md-stem ...) 的 cdr）→ 多行文本（无末尾换行）
    ;; 顶层块之间以空行分隔；空文档返回空字符串
    (define (render-md-stem-document body)
      (if (null? body) "" (join-lines (interpose-empty-line (map render-block body))))
    ) ;define

    ;; 在相邻块的行列表之间插入一个空行；空行列表的块跳过
    (define (interpose-empty-line line-lists)
      (let loop
        ((rem line-lists) (acc '()) (first? #t))
        (cond ((null? rem) (reverse acc))
              ((null? (car rem)) (loop (cdr rem) acc first?))
              (first?
                (loop (cdr rem) (append (reverse (car rem)) acc) #f)
              ) ;first?
              (else
                (loop (cdr rem) (append (reverse (car rem)) (cons "" acc)) #f)
              ) ;else
        ) ;cond
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
