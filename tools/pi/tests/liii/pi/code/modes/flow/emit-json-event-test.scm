;; emit-json-event
;; 将事件序列化为 JSON 行并写入标准输出。
;;
;; 语法
;; ----
;; (emit-json-event ev-type . args)
;;
;; 参数
;; ----
;; ev-type : symbol
;; 事件类型，如 session、agent_start、turn_start、message_start 等。
;; args : any
;; 事件负载参数，按事件类型解释。
;;
;; 返回值
;; ----
;; unspecified
;; 无返回值；事件以 JSON 行形式写入标准输出。

(import (scheme base)
  (liii check)
  (liii base)
  (liii json)
  (liii string)
  (liii pi code modes flow)
) ;import

(check-set-mode! 'report-failed)

;; 统计字符串中的换行符个数
(define (count-newlines s)
  (let loop
    ((i 0) (n 0))
    (if (>= i (string-length s))
      n
      (loop (+ i 1) (if (char=? (string-ref s i) #\newline) (+ n 1) n))
    ) ;loop
  ) ;let
) ;define

;; 1. session 事件：输出单行 JSON，含事件类型与工作目录
(let* ((out (with-output-to-string (lambda () (emit-json-event 'session "/tmp/workdir"))))
       (obj (string->json (string-trim out)))
      ) ;
  (check (cdr (assoc "type" obj)) => "session")
  (check (cdr (assoc "cwd" obj)) => "/tmp/workdir")
  ;; 单行输出：整段 JSON 只占一行
  (check (count-newlines out) => 1))

;; 2. 未知事件类型：不输出任何内容
(let ((out (with-output-to-string (lambda () (emit-json-event 'unknown_event "x")))))
  (check (string-length out) => 0))

(check-report)
