;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

;; 会话历史重建：把目标会话当前有效分支的条目转换为可见的 TUI 卡片。
;;
;; 展示与模型上下文共用同一套投影语义（当前分支、压缩裁剪、context_edit）：
;; 条目集合由 (liii pi code core session-projection) 的
;; pi-build-session-context-entries 给出，本库只做"条目 → 卡片"的转换，
;; 不另写一套只供显示、会与模型恢复规则分叉的 JSONL 遍历算法。
;;
;; 重建是只读的：不重放 agent 事件、不重新执行工具、不发送模型请求，
;; 也不追加或改写会话条目。
(define-library (liii pi code modes interactive app-history)
  (export pi-history-build-cards pi-app-rebuild-history!)
  (import (scheme base)
    (scheme file)
    (liii string)
    (liii list)
    (liii tui)
    (liii pi code core session-projection)
    (liii pi code core compaction session)
    (liii pi code modes interactive state)
    (liii pi code modes interactive render)
    (liii pi code modes interactive views)
    (liii pi code modes interactive bash)
  ) ;import
  (begin

    ;; 字段访问：兼容字符串键与符号键（会话条目由 JSON 解析而来，键为字符串）
    (define (%ref obj key)
      (let* ((str-key (if (symbol? key) (symbol->string key) key))
             (sym-key (if (string? key) (string->symbol key) key))
             (pair (or (assoc str-key obj) (assoc sym-key obj)))
            ) ;
        (if pair (cdr pair) #f)
      ) ;let*
    ) ;define

    ;; JSON true 解析为符号 true，布尔字段需同时兼容符号与 #t
    (define (%json-true? v)
      (or (eq? #t v) (eq? v 'true))
    ) ;define

    ;; 安全取字段：非 alist 时返回 #f（损坏条目不应让整个重建崩溃）
    (define (%safe-ref obj key)
      (if (and (pair? obj) (list? obj)) (%ref obj key) #f)
    ) ;define

    ;; 内容块列表：JSON 数组解析为 vector，统一转为 list
    (define (%blocks content)
      (cond ((vector? content) (vector->list content))
            ((list? content) content)
            (else '())
      ) ;cond
    ) ;define

    ;; 内容块类型：缺失或非字符串时降级为空串
    (define (%block-type block)
      (let ((t (%safe-ref block "type")))
        (if (string? t) t "")
      ) ;let
    ) ;define

    ;; 单个内容块的文本：text 块取 text，thinking 块取 thinking
    (define (%block-text block)
      (let ((t (%safe-ref block "text")))
        (if (string? t)
          t
          (let ((th (%safe-ref block "thinking")))
            (if (string? th) th "")
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 文本内容：字符串直接返回；块数组取各块的文本并以换行连接
    ;; （user 消息的 content 是字符串，assistant / toolResult 的 content 是块数组）
    (define (%content-text content)
      (cond ((string? content) content)
            (else
              (let ((texts
                      (filter
                        (lambda (b) (and (pair? b) (string? (%block-text b))))
                        (%blocks content)
                      ) ;filter
                    ) ;texts
                   ) ;
                (string-join (map (lambda (b) (%block-text b)) texts) "\n")
              ) ;let
            ) ;else
      ) ;cond
    ) ;define

    ;; 压缩摘要卡片的标记：与真实用户输入区分，明确这是历史摘要而非新消息
    (define %history-compacted-label
      "[compacted] Earlier messages were replaced by this summary:\n\n"
    ) ;define

    ;; 工具结果缺失时的终态文案：不留在 pending，也不伪造执行过程
    (define %history-missing-result "Tool result not found in session history")

    ;; 工具结果索引：toolCallId → (结果文本 是否错误)
    ;; 预扫描一次：结果条目总是晚于调用条目，先建索引才能在创建卡片时定终态
    (define (%collect-tool-results entries)
      (let loop
        ((rest entries) (acc '()))
        (if (null? rest)
          (reverse acc)
          (let* ((entry (car rest))
                 (msg (%safe-ref entry "message"))
                 (role (and (list? msg) (%safe-ref msg "role")))
                 (id (and (list? msg) (%safe-ref msg "toolCallId")))
                ) ;
            (if (and (equal? role "toolResult") (string? id))
              (loop (cdr rest)
                (if (assoc id acc)
                  acc
                  (let ((value (list (%content-text (%safe-ref msg "content"))
                                 (%json-true? (%safe-ref msg "isError"))
                               ) ;list
                        ) ;value
                       ) ;
                    (cons (cons id value) acc)
                  ) ;let
                ) ;if
              ) ;loop
              (loop (cdr rest) acc)
            ) ;if
          ) ;let*
        ) ;if
      ) ;let
    ) ;define

    ;; 思考卡片：内容按行拆开，首行进构造，其余行追加
    (define (%make-thinking-card text hidden)
      (let ((lines (string-split text "\n")))
        (let ((card (make-thinking-card (car lines) hidden)))
          (for-each (lambda (line) (thinking-card-append-line! card line)) (cdr lines))
          card
        ) ;let
      ) ;let
    ) ;define

    ;; 历史工具卡片：由 assistant 的 toolCall 块创建，按 toolCallId 回填结果
    ;; 同一调用只形成一张卡片，结果不另起无上下文的独立卡片
    (define (%make-history-tool-card block results tools-expanded?)
      (let* ((id (%safe-ref block "id"))
             (name (%safe-ref block "name"))
             (args (%safe-ref block "arguments"))
             (res (and (string? id) (assoc id results)))
            ) ;
        (if (not (string? id))
          #f
          (let* ((text (if res (cadr res) %history-missing-result))
                 (err? (if res (caddr res) #t))
                 (card (make-tool-card (if (string? name) name "tool")
                         args
                         (if err? 'error 'success)
                         text
                       ) ;make-tool-card
                 ) ;card
                ) ;
            (tool-card-set-expanded! card tools-expanded?)
            (cons card id)
          ) ;let*
        ) ;if
      ) ;let*
    ) ;define

    ;; assistant 内容块 → 卡片：text / thinking / toolCall 按原顺序展示，
    ;; 其余块（如图片）安全跳过
    (define (%assistant-block-pairs block results hide-thinking? tools-expanded?)
      (let ((type (%block-type block)))
        (cond
         ((equal? type "text")
          (let ((text (%block-text block)))
            (if (string-null? text) '() (list (cons (make-assistant-card text) #f)))
          ) ;let
         ) ;
         ((equal? type "thinking")
          (let ((text (%block-text block)))
            (if (string-null? text)
              '()
              (list (cons (%make-thinking-card text hide-thinking?) #f))
            ) ;if
          ) ;let
         ) ;
         ((equal? type "toolCall")
          (let ((pair (%make-history-tool-card block results tools-expanded?)))
            (if pair (list pair) '())
          ) ;let
         ) ;
         (else '())
        ) ;cond
      ) ;let
    ) ;define

    ;; 历史 bash 执行卡片：命令、输出与退出码取自会话条目本身，
    ;; 不从投影后的用户文本反解；excludeFromContext 只影响模型上下文，
    ;; 此类历史仍要在 TUI 中可见（且不重新执行命令）
    ;;
    ;; 折叠状态与实时 ! 命令卡片保持一致：同样服从 Ctrl+O 的全局展开偏好，
    ;; 否则同一屏内历史卡片与新卡片会出现两种折叠状态
    (define (%bash-pairs msg tools-expanded?)
      (let ((command (%safe-ref msg "command"))
            (output (%safe-ref msg "output"))
            (code (%safe-ref msg "exitCode"))
            (excluded? (%json-true? (%safe-ref msg "excludeFromContext")))
           ) ;
        (if (not (string? command))
          '()
          (let ((card (make-bash-card command :expanded? tools-expanded? :excluded? excluded?))
               ) ;
            (bash-card-set-output! card (if (string? output) output ""))
            (bash-card-set-complete! card (if (number? code) code 0))
            (list (cons card #f))
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 消息条目 → 卡片：user 复用用户卡片，assistant 逐块展开，
    ;; toolResult 只回填工具卡片而不单独成卡
    (define (%message-pairs msg results hide-thinking? tools-expanded?)
      (let ((role (%safe-ref msg "role")))
        (cond
         ((equal? role "user")
          (let ((text (%content-text (%safe-ref msg "content"))))
            (if (string-null? text) '() (list (cons (make-user-card text) #f)))
          ) ;let
         ) ;
         ((equal? role "assistant")
          (apply append
            (map (lambda (block)
                   (%assistant-block-pairs block results hide-thinking? tools-expanded?)
                 ) ;lambda
              (%blocks (%ref msg "content"))
            ) ;map
          ) ;apply
         ) ;
         ((equal? role "bashExecution") (%bash-pairs msg tools-expanded?))
         (else '())
        ) ;cond
      ) ;let
    ) ;define

    ;; 条目 → 卡片：compaction 形成摘要卡片，message 按角色分派，
    ;; 其它类型（session 头、状态条目、context_edit）不产生可见卡片
    (define (%entry-pairs entry results hide-thinking? tools-expanded?)
      (let ((type (%safe-ref entry "type")))
        (cond
         ((equal? type "compaction")
          (let ((summary (%safe-ref entry "summary")))
            (if (string? summary)
              (list (cons (make-assistant-card (string-append %history-compacted-label summary)) #f)
              ) ;list
              '()
            ) ;if
          ) ;let
         ) ;
         ((equal? type "message")
          (let ((msg (%safe-ref entry "message")))
            (if (list? msg) (%message-pairs msg results hide-thinking? tools-expanded?) '())
          ) ;let
         ) ;
         (else '())
        ) ;cond
      ) ;let
    ) ;define

    ;; pi-history-build-cards：会话条目 → (卡片列表 工具卡片索引)
    ;; 纯函数：不读文件、不改写会话、不触发任何事件
    ;; - cards：按条目顺序排列的展示卡片
    ;; - index：toolCallId → 工具卡片，用于后续事件回填与状态复位
    (define (pi-history-build-cards entries hide-thinking? tools-expanded?)
      (let* ((results (%collect-tool-results entries))
             (pairs
               (apply append
                 (map (lambda (entry) (%entry-pairs entry results hide-thinking? tools-expanded?))
                   entries
                 ) ;map
               ) ;apply
             ) ;pairs
            ) ;
        (values (map car pairs)
          (let loop
            ((rest pairs) (acc '()))
            (if (null? rest)
              (reverse acc)
              (let ((pair (car rest)))
                (loop (cdr rest) (if (cdr pair) (cons (cons (cdr pair) (car pair)) acc) acc))
              ) ;let
            ) ;if
          ) ;let
        ) ;values
      ) ;let*
    ) ;define

    ;; pi-app-rebuild-history!：用目标会话的历史替换当前聊天区
    ;; 返回 #t 表示重建成功（含空历史），#f 表示失败（当前界面保持不变）
    ;; 先完整读取并转换目标历史，再修改聊天区，避免异常留下半屏新旧混合内容
    ;;
    ;; 降级规则与模型上下文恢复一致：条目集合来自同一投影入口，未知类型与
    ;; 断链条目按投影诊断降级（跳过而非崩溃），会话文件保持只读不改写；
    ;; 投影诊断由上下文恢复路径写入 stderr，备用屏渲染期间不再向终端写文本。
    (define (pi-app-rebuild-history! app session-file)
      (and (string? session-file)
        (file-exists? session-file)
        (catch #t
          (lambda ()
            (let* ((entries (pi-read-session-entries session-file)))
              (let-values (((ctx-entries _state _diagnostics) (pi-build-session-context-entries entries)))
                (let-values (((cards index)
                              (pi-history-build-cards ctx-entries
                                (pi-app-hide-thinking? app)
                                (pi-app-tools-expanded? app)
                              ) ;pi-history-build-cards
                             ) ;
                            ) ;
                  ;; 全部转换成功后再替换界面
                  (container-clear! (%pi-app-chat app))
                  (for-each (lambda (card) (container-add! (%pi-app-chat app) card)) cards)
                  ;; 复位旧会话的引用：工具卡片索引、thinking 引用与文本选区
                  (%pi-app-tool-cards-set! app index)
                  (%pi-app-thinking-card-set! app #f)
                  ;; 旧会话的 ! 命令卡片索引一并复位：历史 bash 卡片由条目重建，
                  ;; 不复用旧索引，避免后续 bash_complete 事件回填到已移除的卡片
                  (%pi-app-bash-cards-set! app '())
                  (pi-app-selection-clear! app)
                  ;; 滚动行数缓存失效并定位到末尾
                  (scroll-view-invalidate! (pi-app-scroll-view app))
                  (pi-app-scroll-bottom! app)
                  #t
                ) ;let-values
              ) ;let-values
            ) ;let*
          ) ;lambda
          (lambda args #f)
        ) ;catch
      ) ;and
    ) ;define

  ) ;begin
) ;define-library
