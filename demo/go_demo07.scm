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

(import (scheme base) (liii go) (liii time))

;; 与 go_demo07.go 严格等价的 Goldfish Scheme (liii go) 版本：
;; 展示著名的单通道乒乓模式（Ping-Pong）：利用无缓冲通道的同步交会实现两个独立协程的轮流协作。

;; player 选手协程：从球台接球并回击
;; table 通道为无缓冲通道，保证同一时刻只有一个选手持球（交会机制 Rendezvous）

(define (player name table done)
  (let loop
    ()
    (let ((hits (chan-recv! table)))
      (if (eof-object? hits)
        (chan-send! done name)
        (begin
          (display (string-append "[" name "] 击球: " (number->string hits) "\n"))
          (sleep 0.02)
          (if (>= hits 6)
            (begin
              (chan-close! table)
              (chan-send! done name)
            ) ;begin
            (begin
              (chan-send! table (+ hits 1))
              (loop)
            ) ;begin
          ) ;if
        ) ;begin
      ) ;if
    ) ;let
  ) ;let
) ;define

(define (main)
  (define table (make-chan))
  (define done (make-chan 2))

  (go (player "ping" table done))
  (go (player "pong" table done))

  ;; 裁判开球：发入第 1 球
  (chan-send! table 1)

  ;; 等待两名选手完成比赛并离场
  (chan-recv! done)
  (chan-recv! done)
  (display "比赛圆满结束\n")
) ;define

(main)
