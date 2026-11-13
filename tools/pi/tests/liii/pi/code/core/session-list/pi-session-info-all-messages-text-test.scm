;; pi-session-info-all-messages-text
;; 返回用于会话搜索的全部消息投影文本。
;;
;; 语法
;; ----
;; (pi-session-info-all-messages-text info)
;;
;; 参数
;; ----
;; info : pi-session-info
;; 会话摘要 record。
;;
;; 返回值
;; ----
;; string
;; 用户和助手消息按顺序拼接的搜索文本。
;;

(import (scheme base)
  (scheme char)
  (scheme file)
  (liii check)
  (liii os)
  (liii path)
  (liii string)
  (liii unicode)
  (liii pi code core session-list)
) ;import

(check-set-mode! 'report-failed)


(let ((info (make-pi-session-info "/tmp/session.jsonl" "session-1"
              "/tmp/project" #f "created" "modified" 2 "first question"
              "first question first answer"
            ) ;make-pi-session-info
      ) ;info
     ) ;
  (check (pi-session-info-all-messages-text info)
    =>
    "first question first answer"
  ) ;check
) ;let

;; 含非法 UTF-8 字节的 JSONL：读取后替换为 U+FFFD，可供 string-downcase 安全处理
(let* ((tmp-dir (path-join (path-temp-dir) "gf-pi-session-list-utf8-test"))
       (file (path->string (path-join tmp-dir "bad-utf8.jsonl")))
       (bad-bytes (string (integer->char #x80) (integer->char #xe4)))
      ) ;
  (unless (file-exists? (path->string tmp-dir))
    (mkdir (path->string tmp-dir))
  ) ;unless
  (call-with-output-file file
    (lambda (port)
      (write-string "{\"type\":\"session\",\"id\":\"s-bad\",\"timestamp\":\"2026-10-20T08:00:00.000Z\",\"cwd\":\"/tmp\"}\n"
        port
      ) ;write-string
      (write-string (string-append
                      "{\"type\":\"message\",\"timestamp\":\"2026-10-20T08:00:01.000Z\",\"message\":{\"role\":\"user\",\"content\":\"hello "
                      bad-bytes
                      " world\"}}"
                    ) ;string-append
        port
      ) ;write-string
      (newline port)
    ) ;lambda
  ) ;call-with-output-file
  (let* ((info (pi-read-session-info file))
         (all-text (pi-session-info-all-messages-text info))
        ) ;
    (check-true (pi-session-info? info))
    (check-true (string-contains? (string-downcase (pi-session-info-first-message info)) "hello"))
    (check-true (string-contains? (string-downcase all-text) "world"))
    (check-true (string-contains? all-text (utf8->string #u8(239 191 189))))
  ) ;let*
  (delete-file file)
  (rmdir (path->string tmp-dir))
) ;let*

(check-report)
