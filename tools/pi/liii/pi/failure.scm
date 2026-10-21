;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; pi 端口统一的「子进程失败」策略：诊断文本与「失败 → 工具错误」的转换。
;; 只有这一处实现，各工具（grep/find/install/...）都经由它转换，
;; 避免每个工具各写一套消息格式。
;;
;; 单独成模块还有两个好处：
;; 1. 不必让 (liii pi read)（文件读取工具）依赖 (liii subprocess)；
;; 2. 依赖的是轻量的 (liii subprocess process-error)，不是整个子进程库。

(define-library (liii pi failure)
  (import (scheme base) (liii string) (liii subprocess process-error))
  (export pi-command-failure-detail pi-command-error)
  (begin

    ;; 诊断文本：stderr 优先，其次 stdout，最后退到退出码
    (define (pi-command-failure-detail err)
      (string-trim-right (process-error-message err))
    ) ;define

    ;; 子进程失败 → 工具错误；错误标签与其它 pi-execute-* 保持一致
    (define (pi-command-error tool-name err)
      (error 'error
        (string-append tool-name " failed: " (pi-command-failure-detail err))
      ) ;error
    ) ;define

  ) ;begin
) ;define-library
