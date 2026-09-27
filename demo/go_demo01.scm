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

;; 与 go_demo01.go 等价的 (liii go) 版本：
;; 无缓冲 channel（rendezvous），producer 在后台 worker 线程发送 1..5 后关闭通道，
;; 主线程消费直到读出 eof-object（等价于 Go 的 for-range 在通道关闭后排空退出）

(define ch (make-chan))

(go (ch)
  (let loop
    ((i 1))
    (when (<= i 5)
      (chan-send! ch i)
      (loop (+ i 1))
    ) ;when
  ) ;let
  (chan-close! ch)
) ;go

(let loop
  ()
  (let ((v (chan-recv! ch)))
    (unless (eof-object? v)
      (display "consume: ")
      (display v)
      (newline)
      (loop)
    ) ;unless
  ) ;let
) ;let
