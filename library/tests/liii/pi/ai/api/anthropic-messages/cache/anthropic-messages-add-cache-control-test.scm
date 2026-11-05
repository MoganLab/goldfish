;; anthropic-messages-add-cache-control
;; 给最后一条 user 消息的最后一个 content 块挂 cache_control 断点。
;;
;; 语法
;; ----
;; (anthropic-messages-add-cache-control messages)
;;
;; 参数
;; ----
;; messages : vector
;; Anthropic API 消息向量，每项为含 role/content 的 alist。
;;
;; 返回值
;; ----
;; vector
;; 末条消息为 user 时，其末尾 content 块附加 cache_control 字段
;; （content 为字符串时转为带断点的 text 块数组）；末条为 assistant
;; 或消息为空时不做修改；其余消息一律保持原样。

(import (liii check)
  (liii pi ai api anthropic-messages cache)
) ;import

(check-set-mode! 'report-failed)

;; 1. 末条 user 消息：末块挂断点，前序消息不变
(let* ((msgs (vector
               '(("role" . "user") ("content" . #((("type" . "text") ("text" . "q")))))
               '(("role" . "assistant") ("content" . #((("type" . "text") ("text" . "a")))))
               '(("role" . "user")
                 ("content" . #((("type" . "text") ("text" . "r1"))
                                (("type" . "text") ("text" . "r2")))))
             ) ;vector
       )
       (out (anthropic-messages-add-cache-control msgs)))
  (check (vector-length out) => 3)
  ;; 前两条消息原样（无 cache_control）
  (check (assoc "cache_control"
            (vector-ref (cdr (assoc "content" (vector-ref out 0))) 0))
    =>
    #f
  ) ;check
  (check (assoc "cache_control"
            (vector-ref (cdr (assoc "content" (vector-ref out 1))) 0))
    =>
    #f
  ) ;check
  ;; 末条消息：仅最后一个块挂断点
  (let ((blks (cdr (assoc "content" (vector-ref out 2)))))
    (check (assoc "cache_control" (vector-ref blks 0)) => #f)
    (check (cdr (assoc "cache_control" (vector-ref blks 1)))
      =>
      '(("type" . "ephemeral"))
    ) ;check
  ) ;let
  ;; 原向量不被修改
  (check (assoc "cache_control"
            (vector-ref (cdr (assoc "content" (vector-ref msgs 2))) 1))
    =>
    #f
  ) ;check
) ;let*

;; 2. 末条消息 content 为纯字符串：转为带断点的 text 块数组
(let* ((msgs #((("role" . "user") ("content" . "hi"))))
       (out (anthropic-messages-add-cache-control msgs))
       (blks (cdr (assoc "content" (vector-ref out 0)))))
  (check (vector? blks) => #t)
  (check (cdr (assoc "type" (vector-ref blks 0))) => "text")
  (check (cdr (assoc "text" (vector-ref blks 0))) => "hi")
  (check (cdr (assoc "cache_control" (vector-ref blks 0))) => '(("type" . "ephemeral"))))

;; 3. 末条为 assistant：不做修改
(let* ((msgs #((("role" . "assistant") ("content" . #((("type" . "text") ("text" . "a")))))))
       (out (anthropic-messages-add-cache-control msgs)))
  (check (assoc "cache_control"
            (vector-ref (cdr (assoc "content" (vector-ref out 0))) 0))
    =>
    #f
  ) ;check
) ;let*

;; 4. 空向量与非向量：原样返回
(check (anthropic-messages-add-cache-control #()) => #())
(check (anthropic-messages-add-cache-control "not-a-vector") => "not-a-vector")

(check-report)
