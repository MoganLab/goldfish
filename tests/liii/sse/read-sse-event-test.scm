(import (liii check) (liii sse) (scheme base))

(check-set-mode! 'report-failed)

;; read-sse-event
;; 从输入端口消费并解析下一个完整 Server-Sent Events (SSE) 事件。
;;
;; 语法
;; ----
;; (read-sse-event [port])
;;
;; 参数
;; ----
;; port : input-port? (可选，默认 (current-input-port))
;; 待读取的输入端口。
;;
;; 返回值
;; ----
;; <sse-event> | eof-object
;; 成功解析返回 <sse-event> 记录实例；流结束且无待发射事件时返回 eof-object。
;;
;; 错误处理
;; ----
;; type-error 当传入的 port 不是 input-port? 时。

;; 1. 基础单事件读取
(let* ((p (open-input-string "data: hello world\n\n")) (ev (read-sse-event p)))
  (check-true (sse-event? ev))
  (check (sse-event-event ev) => "message")
  (check (sse-event-data ev) => "hello world")
  (check (sse-event-id ev) => #f)
  (check (sse-event-retry ev) => #f)
  (check-true (eof-object? (read-sse-event p)))
) ;let*

;; 2. 包含 event, data, id, retry 完整字段
(let* ((text "event: chat\nid: 101\nretry: 3000\ndata: {\"delta\":\"hi\"}\n\n")
       (p (open-input-string text))
       (ev (read-sse-event p))
      ) ;
  (check-true (sse-event? ev))
  (check (sse-event-event ev) => "chat")
  (check (sse-event-data ev) => "{\"delta\":\"hi\"}")
  (check (sse-event-id ev) => "101")
  (check (sse-event-retry ev) => 3000)
  (check-true (eof-object? (read-sse-event p)))
) ;let*

;; 3. 注释行自动忽略与连续注释处理
(let* ((text ": this is a comment\n: another comment\n\ndata: after comments\n\n")
       (p (open-input-string text))
       (ev (read-sse-event p))
      ) ;
  (check (sse-event-data ev) => "after comments")
  (check-true (eof-object? (read-sse-event p)))
) ;let*

;; 4. 注释插在字段之间不影响当前事件累积
(let* ((text "event: update\n: heartbeat ping\ndata: payload\n\n")
       (p (open-input-string text))
       (ev (read-sse-event p))
      ) ;
  (check (sse-event-event ev) => "update")
  (check (sse-event-data ev) => "payload")
) ;let*

;; 5. 多行连续 data: 自动以 \n 换行符拼接
(let* ((text "data: first line\ndata: second line\ndata: third line\n\n")
       (p (open-input-string text))
       (ev (read-sse-event p))
      ) ;
  (check (sse-event-data ev) => "first line\nsecond line\nthird line")
) ;let*

;; 6. 冒号后首个空格剥离语义
;; "data: foo" -> "foo", "data:  bar" -> " bar", "data:foo" -> "foo"
(let* ((text "data: foo\n\ndata:  bar\n\ndata:baz\n\n")
       (p (open-input-string text))
       (ev1 (read-sse-event p))
       (ev2 (read-sse-event p))
       (ev3 (read-sse-event p))
      ) ;
  (check (sse-event-data ev1) => "foo")
  (check (sse-event-data ev2) => " bar")
  (check (sse-event-data ev3) => "baz")
  (check-true (eof-object? (read-sse-event p)))
) ;let*

;; 7. CRLF 与 LF 混合换行支持
(let* ((text "event: test\r\ndata: line1\r\ndata: line2\r\n\r\nevent: next\ndata: ok\n\n"
       ) ;text
       (p (open-input-string text))
       (ev1 (read-sse-event p))
       (ev2 (read-sse-event p))
      ) ;
  (check (sse-event-event ev1) => "test")
  (check (sse-event-data ev1) => "line1\nline2")
  (check (sse-event-event ev2) => "next")
  (check (sse-event-data ev2) => "ok")
  (check-true (eof-object? (read-sse-event p)))
) ;let*

;; 8. 多条 SSE 事件流式连续读取直至 EOF
(let* ((text "data: msg1\n\ndata: msg2\n\ndata: msg3\n\n")
       (p (open-input-string text))
       (events '())
      ) ;
  (let loop
    ()
    (let ((ev (read-sse-event p)))
      (unless (eof-object? ev)
        (set! events (cons (sse-event-data ev) events))
        (loop)
      ) ;unless
    ) ;let
  ) ;let
  (check (reverse events) => '("msg1" "msg2" "msg3"))
  ;; 再次读取依然返回 EOF
  (check-true (eof-object? (read-sse-event p)))
) ;let*

;; 9. 空流直接返回 EOF
(let* ((p (open-input-string "")) (ev (read-sse-event p)))
  (check-true (eof-object? ev))
) ;let*

;; 10. 仅有注释或空行的流直接返回 EOF
(let* ((p (open-input-string ": comment only\n\n\n")) (ev (read-sse-event p)))
  (check-true (eof-object? ev))
) ;let*

;; 11. 未以双换行结尾但在 EOF 处存在有效事件
(let* ((p (open-input-string "data: stream-closed-without-trailing-newline"))
       (ev (read-sse-event p))
      ) ;
  (check (sse-event-data ev) => "stream-closed-without-trailing-newline")
  (check-true (eof-object? (read-sse-event p)))
) ;let*

;; 12. 错误处理：非 input-port 传入
(check-catch 'type-error (read-sse-event 123))
(check-catch 'type-error (read-sse-event "not-a-port"))
(check-catch 'type-error (read-sse-event 'port))

;; 13. 多事件与连续注释排查
(let* ((text (string-append ": init connection\n\n" "data: event one\n\n"
               ": heartbeat\n\n"
               "event: custom\ndata: event two line a\ndata: event two line b\n\n"
               "data: event three\n\n"
             ) ;string-append
       ) ;text
       (p (open-input-string text))
      ) ;
  (let loop
    ((count 0) (evs '()))
    (let ((ev (read-sse-event p)))
      (if (eof-object? ev)
        (check (list count (reverse evs))
          =>
          '(3 ("event one" "event two line a\nevent two line b" "event three"))
        ) ;check
        (loop (+ count 1) (cons (sse-event-data ev) evs))
      ) ;if
    ) ;let
  ) ;let
) ;let*

(let* ((orig (current-input-port))
       (text (string-append ": init connection\n\n" "data: event one\n\n"
               ": heartbeat\n\n"
               "event: custom\ndata: event two line a\ndata: event two line b\n\n"
               "data: event three\n\n"
             ) ;string-append
       ) ;text
       (p (open-input-string text))
      ) ;
  (set-current-input-port p)
  (let loop
    ((count 0) (evs '()))
    (let ((ev (read-sse-event)))
      (if (eof-object? ev)
        (check (list count (reverse evs))
          =>
          '(3 ("event one" "event two line a\nevent two line b" "event three"))
        ) ;check
        (loop (+ count 1) (cons (sse-event-data ev) evs))
      ) ;if
    ) ;let
  ) ;let
  (set-current-input-port orig)
) ;let*

(import (liii subprocess) (liii either) (liii string))
(let* ((res (run-either (list "bin/gf"
                          "-e"
                          "(import (liii sse))\n(let loop () (let ((ev (read-sse-event))) (unless (eof-object? ev) (write (sse-event-data ev)) (newline) (loop))))"
                        ) ;list
              :input
              (string-append ": init connection\n\n" "data: event one\n\n"
                ": heartbeat\n\n"
                "event: custom\ndata: event two line a\ndata: event two line b\n\n"
                "data: event three\n\n"
              ) ;string-append
              :stdout 'capture
            ) ;run-either
       ) ;res
      ) ;
  (check-true (either-right? res))
  (check (string-replace (to-right res) "\r\n" "\n")
    =>
    "\"event one\"\n\"event two line a\\nevent two line b\"\n\"event three\"\n#<unspecified>\n"
  ) ;check
) ;let*

(check-report)
