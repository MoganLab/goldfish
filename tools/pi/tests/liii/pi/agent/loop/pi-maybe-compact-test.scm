;; pi-maybe-compact
;; 检查压缩配置并在满足条件时调用压缩过程，返回压缩后的消息历史或原消息列表。
;;
;; 语法
;; ----
;; (pi-maybe-compact compaction msgs)
;;
;; 参数
;; ----
;; compaction : (or list? #f)，包含 compactor 回调过程的配置 alist。
;; msgs : list?，当前待压缩的会话消息列表。
;;
;; 返回值
;; ----
;; list?，压缩后的新消息列表；若无需压缩、未配置或返回无效，则原样返回 msgs。

(import (scheme base) (liii check) (liii pi agent loop))

(check-set-mode! 'report-failed)

;; 1. 配置了有效的 compactor 且成功返回压缩列表
(let* ((mock-compact
         (lambda (msgs) (list '((role . "assistant")
                                (content . "compacted summary"))))
       ) ;mock-compact
       (compaction (list (cons 'compactor mock-compact)))
       (msgs '(((role . "user")) ((role . "assistant")) ((role . "user"))))
       (res (pi-maybe-compact compaction msgs))
      ) ;
  (check (length res) => 1)
  (check (cdr (assoc 'content (car res))) => "compacted summary")
) ;let*

;; 2. compactor 返回空列表或非列表：防御性兜底回退为原 msgs
(let* ((mock-compact (lambda (msgs) '()))
       (compaction (list (cons 'compactor mock-compact)))
       (msgs '(((role . "user"))))
      ) ;
  (check (pi-maybe-compact compaction msgs) => msgs)
) ;let*

;; 3. 未配置 compactor 或 compaction 为 #f / '()：原样返回 msgs
(let ((msgs '(((role . "user")))))
  (check (pi-maybe-compact #f msgs) => msgs)
  (check (pi-maybe-compact '() msgs) => msgs)
) ;let

(check-report)
