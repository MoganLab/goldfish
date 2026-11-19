;; gfproject-load-config-bundle 的用户层合并
;; 验证 tools 三层合并中的用户层，以及 domains 不跨层合并。
;;
;; 语法
;; ----
;; (gfproject-load-config-bundle)
;;
;; 参数
;; ----
;; 无
;;
;; 返回值
;; ----
;; list
;; 配置包 alist，含 lib/user/local 三层路径、tools、domains 及合并结果。
;;
;; 说明
;; ----
;; 三个不同层级各自拥有独立的 gfproject.json：
;; - lib 层：随 Goldfish 分发
;; - user 层：~/.gf/gfproject.json（本测试用隔离 HOME 构造）
;; - local 层：当前项目根
;; domains 不跨层合并：用户层 domains 只通过 user_domains 单独暴露，
;; 不进入 merged_domains（后者仍只合并 lib + local）。

(import (scheme base)
  (scheme process-context)
  (liii check)
  (liii json)
  (liii os)
  (liii path)
  (liii string)
  (liii gfproject config)
) ;import

(check-set-mode! 'report-failed)

(define orig-home (getenv "HOME"))

(let* ((base
         (path->string
           (path-join (path-temp-dir)
             (string-append "gfproject-user-merge-" (number->string (getpid)))
           ) ;path-join
         ) ;path->string
        ) ;base
       (gf-dir (path->string (path-join base ".gf")))
       (user-config (path->string (path-join gf-dir "gfproject.json")))
      ) ;
  (mkdir base)
  (mkdir gf-dir)
  (putenv "HOME" base)

  ;; 用户层只写增量字段：只覆盖 pi 的 tools_dir 与 description，
  ;; 并额外声明一个用户层独有 domain。
  (path-write-text (path user-config)
    (string-append
      "{\"tools\": {\"pi\": {\"tools_dir\": \"~/git/gfcode2\","
      " \"organization\": \"liii\", \"module\": \"pi/code/cli\","
      " \"description\": {\"zh_CN\": \"用户层 pi\"}}},"
      " \"domains\": {\"user-only\": {\"root\": \"user/\"}}}"
    ) ;string-append
  ) ;path-write-text

  (let* ((bundle (gfproject-load-config-bundle))
         (user-tools (bundle-ref bundle "user_tools"))
         (merged-tools (bundle-ref bundle "merged_tools"))
         (user-domains (bundle-ref bundle "user_domains"))
         (merged-domains (bundle-ref bundle "merged_domains"))
         (pi-tool (json-ref merged-tools "pi"))
         (pi-desc (json-ref pi-tool "description"))
        ) ;
    ;; 路径键：用户层单独暴露，且与 local_path 不同
    (check (bundle-ref bundle "user_path") => user-config)

    ;; 用户层 tools 可见，并进入 merged_tools
    (check-true (json-contains-key? user-tools "pi"))
    (check-true (json-contains-key? merged-tools "pi"))
    (check (json-ref pi-tool "tools_dir") => "~/git/gfcode2")
    (check (json-ref pi-tool "organization") => "liii")
    (check (json-ref pi-desc "zh_CN") => "用户层 pi")

    ;; domains 不跨层合并：merged_domains 不含用户层独有 domain
    (check-false (json-contains-key? merged-domains "user-only"))
    ;; 用户层 domains 单独暴露
    (check-true (json-contains-key? user-domains "user-only"))
  ) ;let*

  (delete-file user-config)
  (path-rmdir (path gf-dir))
  (path-rmdir (path base))
) ;let*

(if orig-home (putenv "HOME" orig-home) (unsetenv "HOME"))

(check-report)
