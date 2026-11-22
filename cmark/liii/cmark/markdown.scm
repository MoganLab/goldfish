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

;; md-stem 到 Markdown 的反向转换入口：校验文档头、调用文档渲染器、
;; 统一换行和末尾换行。节点分派细节在 markdown/ 子模块中实现。
(define-library (liii cmark markdown)
  (import (scheme base))
  (import (liii cmark markdown document))
  (import (liii cmark markdown validate))
  (export md-stem->markdown)
  (begin
    ;; 把 (md-stem 块 ...) S 表达式渲染为 Markdown 文本字符串。
    ;; 顶层块以空行分隔，输出统一使用 \n 并以单个换行结尾；
    ;; 非法输入报 md-stem-error。
    (define (md-stem->markdown doc)
      (check-md-stem-document doc)
      (let ((body (render-md-stem-document (cdr doc))))
        (if (string=? body "") "" (string-append body "\n"))
      ) ;let
    ) ;define
  ) ;begin
) ;define-library
