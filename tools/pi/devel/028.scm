(set! *load-path* (append (list "tools/pi" ".") *load-path*))

(import (scheme base)
  (scheme file)
  ;; 显式导入 (scheme process-context)：get-environment-variable 未被 (liii string)
  ;; 等导出，若不显式导入会解析到 define-library 泄漏的破损绑定并段错误（goldfish 内核 bug，任务 038）
  (scheme process-context)
  (liii check)
  (liii string)
  (liii path)
  (liii subprocess)
  (liii pi bash)
  (liii pi)
) ;import

(check-set-mode! 'report-failed)

;; [028] GLM 5.3 系列接入验证 验收脚本
;; 验收点逐条对应 tools/pi/devel/028.md：
;;   1) TDD 单测：glm 模型目录解析（合并/路由/元数据）与 anthropic 链路
;;   2) 模型目录：--provider glm --list-models 列出 glm-5.3 系列（函数级 + CLI 级）
;;   3) 真实冒烟：glm-5.3 纯文本对话（-p，退出码 0，无 fallback 警告）
;;   4) 真实冒烟：glm-5.3-flash --tools bash 工具调用
;;   5) 门禁检查：构建/fmt/check/doc

(define (%run-shell cmd timeout)
  (run (list (pi-find-bash-path) "-c" cmd) :stdin 'null :timeout timeout)
) ;define

;; 运行命令并把 stdout/stderr 落到临时文件，返回 (退出码 . 输出文本)

(define (%run-capture tag cmd timeout)
  (let ((tmp-out (string-append "tmp-028-" tag ".txt")))
    (let ((ret (%run-shell (string-append cmd " > " tmp-out " 2>&1") timeout)))
      ;; 注：path-read-text 读取目录会触发 goldfish 内核段错误，须先用 path-file? 守卫
      (let ((out
              (if (and (file-exists? tmp-out) (path-file? (path tmp-out)))
                (path-read-text tmp-out)
                ""
              ) ;if
            ) ;out
           ) ;
        (when (file-exists? tmp-out)
          (delete-file tmp-out)
        ) ;when
        (cons ret out)
      ) ;let
    ) ;let
  ) ;let
) ;define

;; 真实 agent 目录（模型目录解析函数可接受目录字符串）

(define %agent-dir
  (let ((env (get-environment-variable "PI_CODING_AGENT_DIR")))
    (if (and env (not (string-null? env)))
      env
      (string-append (get-environment-variable "HOME") "/.pi/agent")
    ) ;if
  ) ;let
) ;define

(display "=== 验收点 1: TDD 单测（glm 模型目录解析 + anthropic 链路） ===\n"
) ;display
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/pi-load-models-json-test.scm"
         60
       ) ;%run-shell
  =>
  0
) ;check
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/pi-merge-models-json-into-store-test.scm"
         60
       ) ;%run-shell
  =>
  0
) ;check
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/pi-resolve-model-metadata-test.scm"
         60
       ) ;%run-shell
  =>
  0
) ;check
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/models/route/pi-provider-has-model-test.scm"
         60
       ) ;%run-shell
  =>
  0
) ;check
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/models/route/pi-route-provider-and-model-test.scm"
         60
       ) ;%run-shell
  =>
  0
) ;check
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/models/list/pi-list-models-test.scm"
         60
       ) ;%run-shell
  =>
  0
) ;check
;; glm 复用 anthropic 兼容链路，核心映射函数不得回归
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/anthropic-parse-response-test.scm"
         60
       ) ;%run-shell
  =>
  0
) ;check
(check (%run-shell "bin/gf test tools/pi/tests/liii/pi/anthropic-build-request-body-test.scm"
         60
       ) ;%run-shell
  =>
  0
) ;check

(display "=== 验收点 2: 模型目录解析（glm-5.3 系列命中，含元数据） ===\n"
) ;display
(let* ((store (pi-load-models-store %agent-dir))
       (models-json (pi-load-models-json %agent-dir))
       (merged (pi-merge-models-json-into-store store models-json))
      ) ;
  ;; 2.1 models.json 中 glm 条目被读取
  (check-true
    (pair? (assoc "glm" (cdr (assoc "providers" models-json))))
  ) ;check-true
  ;; 2.2 合并后目录列出 glm 系列
  (let ((rows (pi-list-models merged "glm")))
    (check-true (>= (length rows) 4))
    (let ((ids (map (lambda (r) (cdr (assoc 'model r))) rows)))
      (check-true (pair? (member "glm-5.3" ids)))
      (check-true (pair? (member "glm-5.3-flash" ids)))
      (check-true (pair? (member "glm-5-turbo" ids)))
      (check-true (pair? (member "glm-4.5-air" ids)))
    ) ;let
  ) ;let
  ;; 2.3 pi-resolve-model-metadata 命中正确元数据（glm-5.3 系列 1M ctx / 131072 maxTokens）
  (let ((meta (pi-resolve-model-metadata "glm" "glm-5.3" merged '())))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'context-window meta)) => 1000000)
    (check (cdr (assoc 'max-tokens meta)) => 131072)
    (check (cdr (assoc 'base-url meta)) => "https://open.bigmodel.cn/api/anthropic")
  ) ;let
  (let ((meta (pi-resolve-model-metadata "glm" "glm-5.3-flash" merged '())))
    (check (cdr (assoc 'matched? meta)) => #t)
    (check (cdr (assoc 'context-window meta)) => 1000000)
    (check (cdr (assoc 'max-tokens meta)) => 131072)
  ) ;let
  ;; 2.4 无 --provider 时 glm-5.3 系列跨 provider 自动路由至 glm
  (let-values (((p m) (pi-route-provider-and-model #f "glm-5.3" '() merged '())))
    (check p => "glm")
    (check m => "glm-5.3")
  ) ;let-values
  (let-values (((p m) (pi-route-provider-and-model #f "glm-5.3-flash" '() merged '())))
    (check p => "glm")
    (check m => "glm-5.3-flash")
  ) ;let-values
) ;let*

;; 2.5 CLI 级：--provider glm --list-models 列出 glm-5.3 系列
(let ((rc (%run-capture "list-models" "bin/gf pi --provider glm --list-models" 30)))
  (check (car rc) => 0)
  (check-true (string-contains? (cdr rc) "glm-5.3"))
  (check-true (string-contains? (cdr rc) "glm-5.3-flash"))
  (check-true (string-contains? (cdr rc) "glm-5-turbo"))
  (check-true (string-contains? (cdr rc) "glm-4.5-air"))
  (check-false (string-contains? (cdr rc) "Unknown provider"))
) ;let

(display "=== 验收点 3: 真实冒烟（glm-5.3 纯文本对话） ===\n")
(let ((rc (%run-capture "smoke-text"
            "bin/gf pi --provider glm --model glm-5.3 -p \"只回复四个字符：glm-ok\""
            120
          ) ;%run-capture
      ) ;rc
     ) ;
  (check (car rc) => 0)
  (check-true (string-contains? (cdr rc) "glm-ok"))
  ;; 模型目录命中后不再出现 fallback 警告
  (check-false (string-contains? (cdr rc) "not found in models store"))
) ;let

(display "=== 验收点 4: 真实冒烟（glm-5.3-flash --tools bash 工具调用） ===\n"
) ;display
(let ((rc (%run-capture "smoke-tools"
            "echo \"hello from stdin\" | bin/gf pi --provider glm --model glm-5.3-flash --tools bash -p \"用 bash 工具执行 echo 028-tool-ok，然后只回复该命令的输出\""
            180
          ) ;%run-capture
      ) ;rc
     ) ;
  (check (car rc) => 0)
  (check-true (string-contains? (cdr rc) "028-tool-ok"))
  (check-false (string-contains? (cdr rc) "not found in models store"))
) ;let

(display "=== 验收点 5: 门禁检查 ===\n")
(check (%run-shell "xmake b goldfish" 600) => 0)
(check (%run-shell "bin/gf fmt" 120) => 0)
;; 注：验收阶段工作区有未提交改动，git diff --exit-code 不在此断言，
;; 以 fmt/check/doc 构建保证；git diff 检查由 Claude 在两次 commit 之间执行。
(check (%run-shell "bin/gf check" 120) => 0)
(check (%run-shell "bin/gf doc --build-json" 60) => 0)

(check-report)
