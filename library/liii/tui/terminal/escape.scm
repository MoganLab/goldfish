;;
;; Copyright (c) 2025 Mario Zechner
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;

(define-library (liii tui terminal escape)
  (export %escape-bytes-complete? %escape-frame-merge %read-escape-bytes)
  (import (scheme base) (liii os))
  (begin

    ;; 回填队列：帧合并时多读到的字节放回这里，对上层完全透明
    (define %pending-bytes '())

    ;; 统一的控制台字节来源：优先取回填字节，再取真实控制台。
    ;; timeout-ms 为 0 时纯非阻塞。返回 #f 表示当前没有可用字节。
    (define (%console-next-byte timeout-ms)
      (if (pair? %pending-bytes)
        (let ((b (car %pending-bytes)))
          (set! %pending-bytes (cdr %pending-bytes))
          b
        ) ;let
        (if (console-kbhit? timeout-ms)
          (let ((b (console-getch)))
            (if (and b (exact-integer? b)) b #f)
          ) ;let
          #f
        ) ;if
      ) ;if
    ) ;define

    ;; CSI 终结字节（0x40~0x7E）：序列到此结束
    (define (%csi-final-byte? b)
      (and (integer? b) (>= b 64) (<= b 126))
    ) ;define

    ;; 转义序列是否已完整（bytes 按到达顺序排列）：
    ;;   ESC [ ... <终结字节>  → CSI 序列（鼠标 SGR 事件即此类）
    ;;   ESC O <终结字节>      → SS3 序列
    ;;   ESC <字符>            → 单字符转义
    ;; 序列完整即可停止读取：否则每个转义序列都要在末尾多等一次超时
    ;; （鼠标滚轮事件因此凭空多出 10ms 延迟）。
    (define (%escape-bytes-complete? bytes)
      (let ((len (length bytes)))
        (cond ((< len 2) #f)
              ((= (cadr bytes) 91)
               (and (> len 2) (%csi-final-byte? (list-ref bytes (- len 1))))
              ) ;
              ((= (cadr bytes) 79) (> len 2))
              (else #t)
        ) ;cond
      ) ;let
    ) ;define

    ;; ESC CR 之后尾随的 LF 属于同一个逻辑按键：部分终端把 Alt+Enter
    ;; 发成 \x1b\r\n 三字节。若在 \x1b\r 处就判定帧完整，裸 LF 会留在
    ;; 队列里，下一轮被解析成 ctrl-j，在已清空的输入框里打出幽灵换行
    ;; （任务 1317）。这里在 10ms 窗口内尝试并入 LF；窗口内到达的不是
    ;; LF 时通过第二个返回值交回调用方回填，不丢键。
    ;; 返回 (values 合并后的字节列表 需要回填的字节或 #f)。
    (define (%escape-frame-merge bytes next-byte)
      (if (and (= (length bytes) 2) (= (cadr bytes) 13))
        (let ((b (next-byte)))
          (cond ((not b) (values bytes #f))
                ((= b 10) (values (append bytes (list b)) #f))
                (else (values bytes b))
          ) ;cond
        ) ;let
        (values bytes #f)
      ) ;if
    ) ;define

    ;; 读取一个转义序列的字节：按到达顺序累积，序列完整即返回
    (define (%read-escape-bytes)
      (let loop
        ((bytes (list 27)))
        (let ((b (%console-next-byte 10)))
          (if (not b)
            bytes
            (let ((bytes (append bytes (list b))))
              (if (%escape-bytes-complete? bytes)
                (let-values (((merged leftover)
                              (%escape-frame-merge bytes (lambda () (%console-next-byte 10)))
                             ) ;
                            ) ;
                  (when leftover
                    (set! %pending-bytes (cons leftover %pending-bytes))
                  ) ;when
                  merged
                ) ;let-values
                (loop bytes)
              ) ;if
            ) ;let
          ) ;if
        ) ;let
      ) ;let
    ) ;define

  ) ;begin
) ;define-library
