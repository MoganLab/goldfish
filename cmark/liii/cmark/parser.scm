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

(define-library (liii cmark parser)
  ;; 生产级门面：只 re-export 生产代码消费的 3 个解析入口
  ;; （块级解析的围栏/缩进代码块、行内解析），其余解析函数
  ;; 由测试直接 import 子模块（block-parser / inline-parser）。
  (import (scheme base))
  (import (liii cmark block parser))
  (import (liii cmark inline parser))
  (export parse-inlines)
  (export parse-fenced-code-block)
  (export parse-indented-code-block)
) ;define-library
