;;
;; Copyright (C) 2026 The Goldfish Scheme Authors
;;
;; Licensed under the MIT License
;;

(define-library (liii pi code all-in-one loader)
  (import (scheme base)
    (scheme file)
    (liii list)
    (liii path)
    (liii string)
    (liii json)
    (liii pi code all-in-one types)
    (liii pi code config user-fs)
  ) ;import
  (export resolve-all-in-one-config-path
    all-in-one-load-config
    ensure-all-in-one-config!
  ) ;export
  (begin

    ;; 解析 all-in-one.json 配置文件路径。
    ;; 缺省或传入 #f 时返回默认路径 ~/.gf/agent/all-in-one.json，
    ;; 显式指定非空配置目录时返回该目录下的 all-in-one.json。
    (define (resolve-all-in-one-config-path . opt-config-dir)
      (let ((base-dir (if (and (pair? opt-config-dir) (car opt-config-dir))
                        (car opt-config-dir)
                        (path-join (path-home) ".gf" "agent")
                      ) ;if
            ) ;base-dir
           ) ;
        (path->string (path-join base-dir "all-in-one.json"))
      ) ;let
    ) ;define

    ;; 解析 chains 数组，返回有效 <chain-def> 列表；格式非法时返回 #f
    (define (%parse-chains cfg-path raw-chains)
      (let ((chains-list (pi-user-vector->list raw-chains)))
        (if (not (list? chains-list))
          (begin
            (pi-user-warn (string-append cfg-path ": chains 必须是数组"))
            #f
          ) ;begin
          (filter (lambda (x) x) (map json->chain-def chains-list))
        ) ;if
      ) ;let
    ) ;define

    ;; 解析 JSON 布尔值（支持 #t/#f 与符号 'true/'false）
    (define (%json-boolean raw default-val)
      (cond ((boolean? raw) raw)
            ((eq? raw 'true) #t)
            ((eq? raw 'false) #f)
            (else default-val)
      ) ;cond
    ) ;define

    ;; 解析秒或毫秒数（秒数乘以 1000 转换为内部毫秒数；支持多种秒级键名）
    (define (%parse-duration-ms data sec-keys ms-key default-ms)
      (let loop
        ((keys (if (list? sec-keys) sec-keys (list sec-keys))))
        (if (pair? keys)
          (let ((val (pi-user-dict-ref data (car keys) #f)))
            (if (and (number? val) (> val 0))
              (inexact->exact (round (* val 1000)))
              (loop (cdr keys))
            ) ;if
          ) ;let
          (let ((ms (pi-user-dict-ref data ms-key #f)))
            (if (and (number? ms) (> ms 0)) ms default-ms)
          ) ;let
        ) ;if
      ) ;let
    ) ;define

    ;; 从合法的根 JSON 对象构建 <all-in-one-cfg> 记录
    (define (%build-cfg cfg-path data)
      (let* ((cont-raw (pi-user-dict-ref data "continuous" #t))
             (cont (%json-boolean cont-raw #t))
             (max-wait (%parse-duration-ms data '("maxWait" "maxWaitSeconds") "maxWaitMs" 3600000)
             ) ;max-wait
             (retry-interval (%parse-duration-ms data
                               '("retryInterval" "cooldown"
                                 "retryIntervalSeconds" "cooldownSeconds")
                               "retryIntervalMs"
                               60000
                             ) ;%parse-duration-ms
             ) ;retry-interval
             (probe-timeout (%parse-duration-ms data
                              '("probeTimeout" "probeTimeoutSeconds")
                              "probeTimeoutMs"
                              8000
                            ) ;%parse-duration-ms
             ) ;probe-timeout
             (raw-chains (pi-user-dict-ref data "chains" '()))
             (chains (%parse-chains cfg-path raw-chains))
            ) ;
        (and chains
          (make-all-in-one-cfg cont max-wait retry-interval probe-timeout chains)
        ) ;and
      ) ;let*
    ) ;define

    ;; %default-all-in-one-json-text
    ;; 默认配置 JSON 文本：双链定义、秒级探活与冷却时间
    (define (%default-all-in-one-json-text)
      (string-append "{\n" "  \"provider\": \"all-in-one\",\n"
        "  \"name\": \"高可用长程推进总线\",\n"
        "  \"continuous\": true,\n" "  \"maxWait\": 3600,\n"
        "  \"retryInterval\": 60,\n" "  \"probeTimeout\": 8,\n"
        "  \"chains\": [\n" "    {\n" "      \"id\": \"coding-fast\",\n"
        "      \"name\": \"快速\",\n" "      \"nodes\": [\n"
        "        {\"provider\": \"google\", \"model\": \"gemini-3.8-flash\", \"thinkingLevel\": \"medium\"},\n"
        "        {\"provider\": \"minimax-cn\", \"model\": \"MiniMax-M3.1-Flash\", \"thinkingLevel\": \"medium\"},\n"
        "        {\"provider\": \"deepseek\", \"model\": \"deepseek-v4-pro\", \"thinkingLevel\": \"high\"}\n"
        "      ]\n" "    },\n" "    {\n"
        "      \"id\": \"coding-high-quality\",\n"
        "      \"name\": \"高质量\",\n" "      \"nodes\": [\n"
        "        {\"provider\": \"kimi-coding\", \"model\": \"k3.1-pro\", \"thinkingLevel\": \"high\"},\n"
        "        {\"provider\": \"zai-coding-cn\", \"model\": \"glm-5.3\", \"thinkingLevel\": \"high\"}\n"
        "      ]\n" "    }\n" "  ]\n" "}\n"
      ) ;string-append
    ) ;define

    ;; 确保默认 all-in-one.json 出现在指定配置目录下（默认 ~/.gf/agent/all-in-one.json）。
    ;; 若文件不存在，则自动原子写入默认双链配置文件；若已存在则保留用户已有配置。
    (define (ensure-all-in-one-config! . opt-config-dir)
      (let* ((cfg-path (apply resolve-all-in-one-config-path opt-config-dir)))
        (if (file-exists? cfg-path)
          #f
          (let ((dir (path-parent (path cfg-path))))
            (path-mkdir dir :parents #t :exist-ok #t)
            (catch #t
              (lambda () (path-write-text cfg-path (%default-all-in-one-json-text)) #t)
              (lambda args #f)
            ) ;catch
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

    ;; all-in-one-load-config
    ;; 加载并校验 all-in-one.json 配置文件。
    ;; 若文件不存在，安全返回 #f。
    ;; 若文件语法错误或根 provider 不是 "all-in-one"，告警后返回 #f。
    ;; 成功时返回 <all-in-one-cfg> 记录。
    (define (all-in-one-load-config . opt-config-dir)
      (when (or (null? opt-config-dir) (not (car opt-config-dir)))
        (ensure-all-in-one-config!)
      ) ;when
      (let* ((cfg-path (apply resolve-all-in-one-config-path opt-config-dir)))
        (if (not (file-exists? cfg-path))
          #f
          (let ((data (pi-user-read-json cfg-path)))
            (cond ((not (pair? data))
                   (pi-user-warn (string-append cfg-path ": JSON 解析失败或根节点非对象")
                   ) ;pi-user-warn
                   #f
                  ) ;
                  ((not (equal? (pi-user-dict-ref data "provider" #f) "all-in-one"))
                   (pi-user-warn (string-append cfg-path ": 根节点 provider 必须是 \"all-in-one\"")
                   ) ;pi-user-warn
                   #f
                  ) ;
                  (else (%build-cfg cfg-path data))
            ) ;cond
          ) ;let
        ) ;if
      ) ;let*
    ) ;define

  ) ;begin
) ;define-library
