;; pi-user-model-exists?
;; 判断模型是否存在于内置目录或用户模型文件中。
;;
;; 语法
;; ----
;; (pi-user-model-exists? config-dir p-id m-id)
;;
;; 参数
;; ----
;; config-dir : string
;; 用户配置目录。
;; p-id : string
;; provider id。
;; m-id : string
;; 模型 id。
;;
;; 返回值
;; ----
;; boolean
;; 模型存在时返回 #t，否则返回 #f。

(import (scheme base)
  (liii check)
  (liii path)
  (liii shutil)
  (liii pi code config user-models)
) ;import

(check-set-mode! 'report-failed)

(define FIXTURE
  (if (file-exists? "tools/pi/tests/fixtures/user-config")
    "tools/pi/tests/fixtures/user-config"
    "tests/fixtures/user-config"
  ) ;if
) ;define

;; 内置模型
(check (pi-user-model-exists? (string-append FIXTURE "/full")
         "google"
         "gemini-3.8-flash"
       ) ;pi-user-model-exists?
  =>
  #t
) ;check

;; 用户模型文件中的模型
(check (pi-user-model-exists? (string-append FIXTURE "/full") "kimi-coding" "k3.1-pro")
  =>
  #t
) ;check

;; 不存在的模型
(check (pi-user-model-exists? (string-append FIXTURE "/full")
         "kimi-coding"
         "no-such-model"
       ) ;pi-user-model-exists?
  =>
  #f
) ;check

;; ollama 本地动态模型始终为 #t
(check (pi-user-model-exists? (string-append FIXTURE "/full")
         "ollama"
         "rafw007/qwen35-claude-coder:9b"
       ) ;pi-user-model-exists?
  =>
  #t
) ;check

;; all-in-one 链式伪模型存在性
(let ((tmp-dir (path->string (path-join (path-temp-dir) "pi-aio-exists-test"))))
  (when (file-exists? tmp-dir)
    (rmtree tmp-dir #t)
  ) ;when
  (path-mkdir tmp-dir :parents #t :exist-ok #t)
  (path-write-text (path->string (path-join tmp-dir "all-in-one.json"))
    "{\"provider\":\"all-in-one\",\"chains\":[{\"id\":\"coding-deep\",\"nodes\":[{\"provider\":\"p\",\"model\":\"m\"}]}]}"
  ) ;path-write-text
  (check (pi-user-model-exists? tmp-dir "all-in-one" "coding-deep") => #t)
  (check (pi-user-model-exists? tmp-dir "all-in-one" "no-such-chain") => #f)
  (rmtree tmp-dir #t)
) ;let

(check-report)
