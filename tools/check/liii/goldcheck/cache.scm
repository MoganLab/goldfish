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
;; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
;; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

;; gf check 规则级缓存：空文件存在即表示该规则检查通过。

(define-library (liii goldcheck cache)
  (import (scheme base) (liii base) (liii goldtool cache))
  (export rule-cache-path rule-cache-hit? rule-cache-touch)
  (begin

    (define (rule-cache-path rule-name limit file-path)
      (goldtool-cache-path "check" file-path rule-name (number->string limit))
    ) ;define

    (define (rule-cache-hit? rule-name limit file-path)
      (goldtool-cache-hit? (rule-cache-path rule-name limit file-path))
    ) ;define

    (define (rule-cache-touch rule-name limit file-path)
      (goldtool-cache-touch (rule-cache-path rule-name limit file-path))
    ) ;define

  ) ;begin
) ;define-library
