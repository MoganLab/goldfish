;; (liii goldfmt-config) 模块测试文件
;;
;; 重点测试 lang-binary 对 binary / binary-linux 等字段的支持：
;; 既接受单个字符串，也接受字符串数组（取第一个存在且可执行的）。

(import (liii check)
  (scheme file)
  (liii base)
  (liii os)
  (liii path)
  (liii string)
  (liii json)
  (liii semver)
  (liii subprocess)
  (liii goldfmt-config)
) ;import

(check-set-mode! 'report-failed)

;; 辅助：构造一个三平台 binary 字段都填相同值的配置，使测试在任意平台读到一致结果。

(define (make-cfg binary-value)
  (string->json (string-append "{\"cpp\": {" "\"binary-linux\": " binary-value
                  "," "\"binary-windows\": " binary-value ","
                  "\"binary-macos\": " binary-value "}}"
                ) ;string-append
  ) ;string->json
) ;define

;; POSIX 平台必定存在且可执行的解释器；用于“存在的显式路径”用例。
;; Windows 上 /bin/sh 不存在，这些用例在该平台无意义，按平台跳过。

(define posix? (not (os-windows?)))

;; ---- 用例 1：未配置 binary，回退默认 "clang-format" ----
(let ((cfg (string->json "{\"cpp\": {}}")))
  (check (lang-binary 'cpp cfg) => "clang-format")
) ;let

;; ---- 用例 2：裸命令名（不含分隔符）视为可用，直接返回 ----
(let ((cfg (make-cfg "\"clang-format-19\"")))
  (check (lang-binary 'cpp cfg) => "clang-format-19")
) ;let

;; ---- 用例 3：单个存在的显式路径，返回它（仅 POSIX） ----
(when posix?
  (let ((cfg (make-cfg "\"/bin/sh\"")))
    (check (lang-binary 'cpp cfg) => "/bin/sh")
  ) ;let
) ;when

;; ---- 用例 4：单个不存在的显式路径，回退默认（仅 POSIX） ----
(when posix?
  (let ((cfg (make-cfg "\"/no/such/clang-format\"")))
    (check (lang-binary 'cpp cfg) => "clang-format")
  ) ;let
) ;when

;; ---- 用例 5：数组——首项不存在，落到存在的次项（仅 POSIX） ----
(when posix?
  (let ((cfg (make-cfg "[\"/no/such/clang-format\", \"/bin/sh\"]")))
    (check (lang-binary 'cpp cfg) => "/bin/sh")
  ) ;let
) ;when

;; ---- 用例 6：数组——全是不存在的显式路径，回退默认（仅 POSIX） ----
(when posix?
  (let ((cfg (make-cfg "[\"/no/such/a\", \"/no/such/b\"]")))
    (check (lang-binary 'cpp cfg) => "clang-format")
  ) ;let
) ;when

;; ---- 用例 7：数组——含裸命令名，视为可用并选中 ----
(let ((cfg (make-cfg "[\"/no/such/a\", \"clang-format-19\"]")))
  (check (lang-binary 'cpp cfg) => "clang-format-19")
) ;let

;; ---- 用例 8：数组——首个裸命令名优先于后续存在的显式路径 ----
(when posix?
  (let ((cfg (make-cfg "[\"clang-format-19\", \"/bin/sh\"]")))
    (check (lang-binary 'cpp cfg) => "clang-format-19")
  ) ;let
) ;when

;; ---- 用例 9：os-bin 优先级高于 generic binary ----
;; 当前平台的 os 字段命中（裸命令名），即使 generic 是另一个值也以 os 为准。
(let ((cfg (string->json (string-append "{\"cpp\": {\"binary\": \"/no/such/generic\","
                           "\"binary-linux\": \"clang-format-os\","
                           "\"binary-windows\": \"clang-format-os\","
                           "\"binary-macos\": \"clang-format-os\"}}"
                         ) ;string-append
           ) ;string->json
      ) ;cfg
     ) ;
  (check (lang-binary 'cpp cfg) => "clang-format-os")
) ;let

;; ---- suffix：不带点补点，已带点原样保留 ----
(let ((cfg (string->json "{\"cpp\": {\"suffix\": [\"cpp\", \".hpp\"]}}")))
  (check (lang-suffixes 'cpp cfg) => '(".cpp" ".hpp"))
) ;let

;; ---- suffix：空串项被丢弃（不变成误匹配一切的 "."） ----
(let ((cfg (string->json "{\"cpp\": {\"suffix\": [\"\", \"cpp\", \"\"]}}")))
  (check (lang-suffixes 'cpp cfg) => '(".cpp"))
) ;let

;; ---- suffix：未配置时回退默认后缀表 ----
(let ((cfg (string->json "{\"cpp\": {}}")))
  (check (lang-suffixes 'cpp cfg) => '(".hpp" ".cpp" ".h" ".c" ".cc" ".cxx"))
) ;let

;; ---- goldfish_version 字段读取 ----
(check (fmt-config-goldfish-version (string->json "{}")) => #f)
(check (fmt-config-goldfish-version (string->json "{\"goldfish_version\": \"18.11.35\"}")
       ) ;fmt-config-goldfish-version
  =>
  "18.11.35"
) ;check
(check (fmt-config-goldfish-version (string->json "{\"goldfish_version\": 18}"))
  =>
  #f
) ;check
(check (fmt-config-goldfish-version (string->json "{\"goldfish_version\": true}"))
  =>
  #f
) ;check
(check (fmt-config-goldfish-version (string->json "{\"goldfish_version\": [\"18.11.35\"]}")
       ) ;fmt-config-goldfish-version
  =>
  #f
) ;check
(check (fmt-config-goldfish-version (string->json "{\"goldfish_version\": null}"))
  =>
  #f
) ;check
(check (fmt-config-goldfish-version #f) => #f)

;; ---- goldfish_version 语义化版本比对与合法性验证 ----
(let ((ver
        (fmt-config-goldfish-version (string->json (string-append "{\"goldfish_version\": \"" (version) "\"}"))
        ) ;fmt-config-goldfish-version
      ) ;ver
     ) ;
  (check (and (semver-valid? ver) (semver=? (version) ver)) => #t)
) ;let

(let ((ver (fmt-config-goldfish-version (string->json "{\"goldfish_version\": \"99.0.0\"}")
           ) ;fmt-config-goldfish-version
      ) ;ver
     ) ;
  (check (and (semver-valid? ver) (semver=? (version) ver)) => #f)
) ;let

(let ((ver (fmt-config-goldfish-version (string->json "{\"goldfish_version\": \"0.1.0\"}"))
      ) ;ver
     ) ;
  (check (and (semver-valid? ver) (semver=? (version) ver)) => #f)
) ;let

(let ((ver (fmt-config-goldfish-version (string->json "{\"goldfish_version\": \"invalid-version\"}")
           ) ;fmt-config-goldfish-version
      ) ;ver
     ) ;
  (check (semver-valid? ver) => #f)
) ;let

(let ((ver (fmt-config-goldfish-version (string->json "{\"goldfish_version\": \"1.2.3.4\"}")
           ) ;fmt-config-goldfish-version
      ) ;ver
     ) ;
  (check (semver-valid? ver) => #f)
) ;let

(let ((ver (fmt-config-goldfish-version (string->json "{\"goldfish_version\": \"01.0.0\"}")
           ) ;fmt-config-goldfish-version
      ) ;ver
     ) ;
  (check (semver-valid? ver) => #f)
) ;let

;; ---- goldfish_version CLI 门禁端到端验证 ----

(define project-root
  (if (file-exists? "gfproject.json")
    (getcwd)
    (if (file-exists? "../../gfproject.json")
      (path->string
        (path-parent (path-parent (path (getcwd))))
      ) ;path->string
      (getcwd)
    ) ;if
  ) ;if
) ;define

(define (remove-tree target)
  (cond ((path-file? target) (path-unlink target #t))
        ((path-dir? target)
         (let ((entries (path-list-path target)))
           (let loop
             ((i 0))
             (if (< i (vector-length entries))
               (begin
                 (remove-tree (vector-ref entries i))
                 (loop (+ i 1))
               ) ;begin
               #t
             ) ;if
           ) ;let
         ) ;let
         (path-rmdir target)
        ) ;
  ) ;cond
) ;define

(define sandbox-dir
  (path->string
    (path-join (path-temp-dir)
      (string-append "goldfmt-version-test-" (number->string (getpid)))
    ) ;path-join
  ) ;path->string
) ;define

(run-set! 'gf (string-append project-root "/bin/gf"))

(dynamic-wind
  (lambda ()
    (remove-tree sandbox-dir)
    (mkdir sandbox-dir)
    (path-write-text (path (string-append sandbox-dir "/gfproject.json"))
      "{\"name\": \"test-project\"}\n"
    ) ;path-write-text
  ) ;lambda
  (lambda ()
    ;; 场景 1：版本相等，允许执行，退出码为 0
    (path-write-text (path (string-append sandbox-dir "/gf_fmt.json"))
      (string-append "{\"goldfish_version\": \""
        (version)
        "\", \"scheme\": {\"suffix\": [\"scm\"], \"path\": [\".\"]}}\n"
      ) ;string-append
    ) ;path-write-text
    (let-values (((out err code)
                  (run-values (list 'gf "fmt" "--check")
                    :cwd    sandbox-dir
                    :stdout 'capture
                    :stderr 'stdout
                  ) ;run-values
                 ) ;
                ) ;
      (check code => 0)
    ) ;let-values

    ;; 场景 2：版本号更大（99.0.0），阻止执行，打印英文错误并退出 1
    (path-write-text (path (string-append sandbox-dir "/gf_fmt.json"))
      "{\"goldfish_version\": \"99.0.0\", \"scheme\": {\"suffix\": [\"scm\"], \"path\": [\".\"]}}\n"
    ) ;path-write-text
    (let-values (((out err code)
                  (run-values (list 'gf "fmt" "--check")
                    :cwd    sandbox-dir
                    :stdout 'capture
                    :stderr 'stdout
                  ) ;run-values
                 ) ;
                ) ;
      (check code => 1)
      (check (string-contains? out
               "error: gf_fmt.json requires Goldfish version 99.0.0, but current version is "
             ) ;string-contains?
        =>
        #t
      ) ;check
    ) ;let-values

    ;; 场景 3：版本号更小（0.1.0），阻止执行，打印英文错误并退出 1
    (path-write-text (path (string-append sandbox-dir "/gf_fmt.json"))
      "{\"goldfish_version\": \"0.1.0\", \"scheme\": {\"suffix\": [\"scm\"], \"path\": [\".\"]}}\n"
    ) ;path-write-text
    (let-values (((out err code)
                  (run-values (list 'gf "fmt" "--check")
                    :cwd    sandbox-dir
                    :stdout 'capture
                    :stderr 'stdout
                  ) ;run-values
                 ) ;
                ) ;
      (check code => 1)
      (check (string-contains? out
               "error: gf_fmt.json requires Goldfish version 0.1.0, but current version is "
             ) ;string-contains?
        =>
        #t
      ) ;check
    ) ;let-values

    ;; 场景 4：非法版本格式，阻止执行，打印英文错误并退出 1
    (path-write-text (path (string-append sandbox-dir "/gf_fmt.json"))
      "{\"goldfish_version\": \"not-a-semver\", \"scheme\": {\"suffix\": [\"scm\"], \"path\": [\".\"]}}\n"
    ) ;path-write-text
    (let-values (((out err code)
                  (run-values (list 'gf "fmt" "--check")
                    :cwd    sandbox-dir
                    :stdout 'capture
                    :stderr 'stdout
                  ) ;run-values
                 ) ;
                ) ;
      (check code => 1)
      (check (string-contains? out
               "error: gf_fmt.json requires Goldfish version not-a-semver, but current version is "
             ) ;string-contains?
        =>
        #t
      ) ;check
    ) ;let-values
  ) ;lambda
  (lambda () (remove-tree sandbox-dir))
) ;dynamic-wind

(check-report)
