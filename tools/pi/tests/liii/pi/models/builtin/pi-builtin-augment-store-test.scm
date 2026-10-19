(import (liii check) (liii pi models builtin))

(check-set-mode! 'report-failed)

;; pi-builtin-augment-store
;; 把内置模型目录补进 models-store，供 --list-models 显示内置模型。

(define (%model-ids entry)
  (let ((models (cdr (assoc "models" (cdr entry)))))
    (map (lambda (m) (cdr (assoc "id" m))) models)))

;; 1. 空 store 时为 minimax 建立完整内置目录 (3 个模型)
(let ((aug (pi-builtin-augment-store '() "minimax")))
  (check-true (pair? (assoc "minimax" aug)))
  (check (%model-ids (assoc "minimax" aug))
    => '("MiniMax-M2.7" "MiniMax-M2.7-highspeed" "MiniMax-M3")))

;; 2. 已有 store 条目时只补齐缺失模型，不重复
(let* ((store
         (list (cons "minimax"
                 (list (cons "models"
                         (vector (list (cons "id" "MiniMax-M2.7")
                                   (cons "baseUrl"
                                     "https://api.minimax.io/anthropic"
                                   ) ;cons
                                 ) ;list
                           ) ;vector
                       ) ;cons
                 ) ;list
               ) ;cons
         ) ;list
       ) ;store
       (aug (pi-builtin-augment-store store "minimax")))
  (check (%model-ids (assoc "minimax" aug))
    => '("MiniMax-M2.7" "MiniMax-M2.7-highspeed" "MiniMax-M3")))

;; 3. minimax-cn 同样获得内置目录 (3 个模型)
(let ((aug (pi-builtin-augment-store '() "minimax-cn")))
  (check-true (pair? (assoc "minimax-cn" aug)))
  (check (length (%model-ids (assoc "minimax-cn" aug))) => 3))

;; 4. 未知 provider 保持不变
(check (pi-builtin-augment-store '() "unknown-provider") => '())

(check-report)
