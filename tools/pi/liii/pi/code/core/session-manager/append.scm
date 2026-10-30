;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;
;; 会话 JSONL 的追加写入层：条目追加、末条 id 查询与状态条目写入。
;; 独立成库既保证 session-manager 主文件不超出行数上限，也让状态条目
;; 复用与消息条目一致的 id / parentId / timestamp 生成与退避重试语义。
(define-library (liii pi code core session-manager append)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii path)
    (liii json)
    (liii uuid)
    (liii time)
    (liii pi ai time)
  ) ;import
  (export append-session-entry!
    find-last-session-message-id
    append-session-state-entry!
  ) ;export
  (begin
    (define (%extract-id-from-line line)
      (let ((trimmed (string-trim line)))
        (if (<= (string-length trimmed) 0)
          #f
          (let ((parsed (catch #t (lambda () (string->json trimmed)) (lambda _ #f))))
            (and parsed (assoc "id" parsed) (cdr (assoc "id" parsed)))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    (define (find-last-session-message-id file-path)
      (let ((p-str (if (string? file-path) file-path (path->string file-path))))
        (if (not (file-exists? p-str))
          #f
          (let ((lines (path-read-lines p-str)) (last-id #f))
            (for-each
              (lambda (line)
                (let ((mid (%extract-id-from-line line)))
                  (when mid
                    (set! last-id mid)
                  ) ;when
                ) ;let
              ) ;lambda
              lines
            ) ;for-each
            last-id
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 追加写入：Windows 下新建文件可能被杀毒/索引短暂锁定导致打开失败，
    ;; 带退避重试（对齐上游 pi 会话写入的容错语义）
    (define (%append-entry-safe! p-str entry-json-str)
      (let loop
        ((attempt 0))
        (or
          (catch #t
            (lambda ()
              (let ((port (open-output-file p-str "a")))
                (display entry-json-str port)
                (newline port)
                (close-output-port port)
                #t
              ) ;let
            ) ;lambda
            (lambda args #f)
          ) ;catch
          (if (< attempt 5)
            (begin
              (sleep 0.05)
              (loop (+ attempt 1))
            ) ;begin
            (error 'io-error (string-append "cannot append session entry: " p-str))
          ) ;if
        ) ;or
      ) ;let
    ) ;define

    (define (append-session-entry! file-path entry-json-str)
      (let ((p-str (if (string? file-path) file-path (path->string file-path))))
        (%append-entry-safe! p-str entry-json-str)
      ) ;let
    ) ;define

    ;; 将状态条目（model_change / thinking_level_change 等）追加到会话文件：
    ;; - id / parentId / timestamp 的生成与消息条目一致，parentId 指向文件中的上一条，
    ;;   保证会话文件始终是一条线性链
    ;; - type 不做白名单校验，未知类型由调用方负责，新增状态无需改会话层
    ;; - 业务字段平铺在顶层，整条记录用 json->string 构造，避免人工拼接
    ;;   导致字段值里的引号、反斜杠产生非法 JSON
    ;; - sfile 为 #f（--no-session）时静默返回 #f，不创建文件
    ;; 返回新条目 id
    (define (%append-state-entry-once! sfile type fields)
      (let* ((sid (uuid4))
             (parent-id (find-last-session-message-id sfile))
             (now-iso (pi-iso-8601-utc-now))
             (entry
               (json->string
                 (append (list (cons "type" type)
                           (cons "id" sid)
                           (cons "parentId" (if parent-id parent-id 'null))
                           (cons "timestamp" now-iso)
                         ) ;list
                   fields
                 ) ;append
               ) ;json->string
             ) ;entry
            ) ;
        (append-session-entry! sfile entry)
        sid
      ) ;let*
    ) ;define

    (define (append-session-state-entry! sfile type fields)
      (if (not sfile)
        #f
        (let loop
          ((attempt 0))
          (or (catch #t
                (lambda () (%append-state-entry-once! sfile type fields))
                (lambda args #f)
              ) ;catch
            (if (< attempt 5)
              (begin
                (sleep 0.05)
                (loop (+ attempt 1))
              ) ;begin
              (error 'io-error
                (string-append "cannot append session state entry: "
                  (if (string? sfile) sfile (path->string sfile))
                ) ;string-append
              ) ;error
            ) ;if
          ) ;or
        ) ;let
      ) ;if
    ) ;define

  ) ;begin
) ;define-library
