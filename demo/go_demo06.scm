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
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
;; License for the specific language governing permissions and limitations
;; under the License.
;;

(import (scheme base) (liii go))

;; 与 go_demo06.go 严格等价的 Goldfish Scheme (liii go) 版本：
;; 展示已关闭通道的接收行为：通道关闭后缓冲数据仍可读取，排空后返回 eof-object。

(define (main)
  ;; 创建容量为 1 的缓冲通道，写入数据后立即关闭
  (define ch (make-chan 1))
  (chan-send! ch 7)
  (chan-close! ch)

  ;; 1. 通道虽已关闭，但缓冲区中仍有残留数据 7，依然可以正常接收
  (display (chan-recv! ch))
  (newline)

  ;; 2. 通道已关闭且缓冲区已排空：
  ;;    在 Scheme 中，通道排空后读取会返回 eof-object（对应 Go 的 ok = false）
  (let* ((raw (chan-recv! ch)) (ok (not (eof-object? raw))) (v (if ok raw 0)))
    (display v)
    (display " ")
    (display ok)
    (newline)
  ) ;let*
) ;define

(main)
