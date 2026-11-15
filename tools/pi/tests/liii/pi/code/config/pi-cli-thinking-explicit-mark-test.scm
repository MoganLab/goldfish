;; resolve-pi-config 的显式思考意图标记
;; 只有 --thinking 真正由用户给出时才标记 thinking-level-explicit? = #t；
;; settings 默认值与节点默认值不会被误标为本次显式覆盖。
;;
;; 语法
;; ----
;; (resolve-pi-config parsed env)
;;
;; 参数
;; ----
;; parsed : alist
;; 解析后的命令行参数，可含 provider、model 与 thinking-level。
;; env : alist
;; 环境变量列表，提供 PI_CODING_AGENT_DIR 指向测试用配置目录。
;;
;; 返回值
;; ----
;; alist
;; 含 thinking-level、thinking-level-explicit? 与 thinking-level-source 的配置。

(import (scheme base)
  (scheme file)
  (liii check)
  (liii os)
  (liii path)
  (liii shutil)
  (liii string)
  (liii pi code config)
) ;import

(check-set-mode! 'report-failed)

(define TMP-DIR
  (path->string
    (path-join (path-temp-dir)
      (string-append "pi-thinking-explicit-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when
(path-mkdir (path (path-join TMP-DIR "models" "stub")) :parents #t :exist-ok #t)
(path-write-text (path->string (path-join TMP-DIR "models" "providers.json")) "[\"stub\"]")
(path-write-text (path->string (path-join TMP-DIR "auth.json"))
  "{\"stub\":{\"type\":\"api_key\",\"key\":\"k\"}}"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "models" "stub.json"))
  "{\"api\":\"openai-completions\",\"baseUrl\":\"http://127.0.0.1:1\",\"models\":[\"one\"]}"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "models" "stub" "one.json"))
  "{\"id\":\"one\",\"name\":\"one\",\"reasoning\":true}"
) ;path-write-text
(path-write-text (path->string (path-join TMP-DIR "settings.json"))
  "{\"defaultThinkingLevel\":\"low\"}"
) ;path-write-text

(define ENV (list (cons "PI_CODING_AGENT_DIR" TMP-DIR)))

(define (cfg-ref cfg key)
  (pi-cfg-ref cfg key 'missing)
) ;define

;; 1. 显式 --thinking high：标记为 #t，来源为 cli，生效值 high
(let ((cfg (resolve-pi-config
             (list (cons 'provider "stub")
               (cons 'model "one")
               (cons 'thinking-level "high")
             ) ;list
             ENV
           ) ;resolve-pi-config
          ) ;
     ) ;
  (check (cfg-ref cfg 'thinking-level) => "high")
  (check (cfg-ref cfg 'thinking-level-explicit?) => #t)
  (check (cfg-ref cfg 'thinking-level-source) => 'cli)
) ;let

;; 2. 无 --thinking：settings 默认值 low 不得被标为显式覆盖
(let ((cfg (resolve-pi-config
             (list (cons 'provider "stub") (cons 'model "one"))
             ENV
           ) ;resolve-pi-config
          ) ;
     ) ;
  (check (cfg-ref cfg 'thinking-level) => "low")
  (check (cfg-ref cfg 'thinking-level-explicit?) => #f)
  (check (cfg-ref cfg 'thinking-level-source) => 'default)
) ;let

;; 3. --thinking 为空串或空白串：同样不视为显式
(let ((cfg (resolve-pi-config
             (list (cons 'provider "stub") (cons 'model "one") (cons 'thinking-level "  "))
             ENV
           ) ;resolve-pi-config
          ) ;
     ) ;
  (check (cfg-ref cfg 'thinking-level-explicit?) => #f)
) ;let

(when (file-exists? TMP-DIR)
  (rmtree TMP-DIR #t)
) ;when

(check-report)
