;; (liii config-parser) test file
;;
;; Tests for config-parser, a Python configparser-like INI configuration library.

;; ==== 函数分类索引 ====

;; 一、构造与读取
;;   make-config-parser      - 创建空的配置解析器
;;   config-read-string      - 从字符串读取配置
;;   config-read-file        - 从文件读取配置

;; 二、查询
;;   config-sections         - 返回所有 section 名列表
;;   config-has-section?     - 检查 section 是否存在
;;   config-has-option?      - 检查 option 是否存在
;;   config-get              - 获取值（字符串）
;;   config-get-int          - 获取整数值
;;   config-get-boolean      - 获取布尔值
;;   config-options          - 获取 section 下所有 option 名
;;   config-items            - 获取 section 下所有 (key . value) 对

;; 三、修改
;;   config-set!             - 设置值
;;   config-add-section!     - 添加 section
;;   config-remove-section!  - 删除 section
;;   config-remove-option!   - 删除 option

;; 四、输出
;;   config-write            - 将配置写出到端口

;; ==== 单元测试 ====

(import (liii check)
  (liii config-parser)
  (liii raw-string)
  (liii base)
  (liii string)
  (scheme base)
) ;import

(check-set-mode! 'report-failed)

;; ==== 测试用 INI 字符串 ====

(define test-ini
  (&- #""
    [DEFAULT]
    ServerAliveInterval = 45
    Compression = yes
    ForwardX11 = yes

    [forge.example]
    User = hg

    [topsecret.server.example]
    Port = 50022
    ForwardX11 = no
    ""
  ) ;&-
) ;define

(define test-ini-simple
  (&- #""
    [database]
    host=localhost
    port=5432
    ""
  ) ;&-
) ;define

;; ==== 1. make-config-parser: 创建空解析器 ====

(let ((config (make-config-parser)))
  (check (config-parser? config) => #t)
  (check (config-sections config) => '())
) ;let

;; ==== 2. config-read-string: 基础读取 ====

(let ((config (make-config-parser)))
  (config-read-string config test-ini-simple)
  (check (config-sections config) => '("database"))
) ;let

;; ==== 3. config-has-section? ====

(let ((config (make-config-parser)))
  (config-read-string config test-ini-simple)
  (check (config-has-section? config "database") => #t)
  (check (config-has-section? config "missing") => #f)
) ;let

;; ==== 4. config-has-option? ====

(let ((config (make-config-parser)))
  (config-read-string config test-ini-simple)
  (check (config-has-option? config "database" "host") => #t)
  (check (config-has-option? config "database" "missing") => #f)
) ;let

;; ==== 5. config-get: 获取值 ====

(let ((config (make-config-parser)))
  (config-read-string config test-ini-simple)
  (check (config-get config "database" "host") => "localhost")
  (check (config-get config "database" "port") => "5432")
) ;let

;; ==== 6. config-get: 前后空白被忽略 ====

(let ((config (make-config-parser)))
  (config-read-string config
    (&- #""
      [section]
        key  =  value
      ""
    ) ;&-
  ) ;config-read-string
  (check (config-get config "section" "key") => "value")
) ;let

;; ==== 7. config-get-int: 获取整数 ====

(let ((config (make-config-parser)))
  (config-read-string config test-ini)
  (check (config-get-int config "topsecret.server.example" "port") => 50022)
  (check (config-get-int config "DEFAULT" "serveraliveinterval") => 45)
) ;let

;; ==== 8. config-get-boolean: 获取布尔值 ====

(let ((config (make-config-parser)))
  (config-read-string config test-ini)
  (check (config-get-boolean config "topsecret.server.example" "forwardx11")
    =>
    #f
  ) ;check
  (check (config-get-boolean config "DEFAULT" "compression") => #t)
) ;let

;; ==== 9. DEFAULT section 继承 ====

(let ((config (make-config-parser)))
  (config-read-string config test-ini)
  ;; forge.example 没有 Compression，应从 DEFAULT 继承
  (check (config-get config "forge.example" "compression") => "yes")
  ;; topsecret.server.example 覆盖了 ForwardX11
  (check (config-get config "topsecret.server.example" "forwardx11") => "no")
) ;let

;; ==== 10. config-sections: 不包含 DEFAULT ====

(let ((config (make-config-parser)))
  (config-read-string config test-ini)
  (check (config-sections config)
    =>
    '("forge.example" "topsecret.server.example")
  ) ;check
) ;let

;; ==== 11. config-options: 获取 section 下所有 option ====

(let ((config (make-config-parser)))
  (config-read-string config test-ini-simple)
  (check (length (config-options config "database")) => 2)
  (check (config-has-option? config "database" "host") => #t)
  (check (config-has-option? config "database" "port") => #t)
) ;let

;; ==== 12. config-options: 包含 DEFAULT 继承的 option ====

(let ((config (make-config-parser)))
  (config-read-string config test-ini)
  ;; forge.example 自己有 User，DEFAULT 有 ServerAliveInterval/Compression/ForwardX11
  (check (config-has-option? config "forge.example" "user") => #t)
  (check (config-has-option? config "forge.example" "compression") => #t)
) ;let

;; ==== 13. config-items: 获取所有键值对 ====

(let ((config (make-config-parser)))
  (config-read-string config test-ini-simple)
  (let ((items (config-items config "database")))
    (check (assoc "host" items) => '("host" . "localhost"))
    (check (assoc "port" items) => '("port" . "5432"))
  ) ;let
) ;let

;; ==== 14. config-set!: 设置值 ====

(let ((config (make-config-parser)))
  (config-read-string config test-ini-simple)
  (config-set! config "database" "host" "127.0.0.1")
  (check (config-get config "database" "host") => "127.0.0.1")
) ;let

;; ==== 15. config-set!: 设置新 option ====

(let ((config (make-config-parser)))
  (config-read-string config test-ini-simple)
  (config-set! config "database" "timeout" "30")
  (check (config-get config "database" "timeout") => "30")
) ;let

;; ==== 16. config-add-section!: 添加新 section ====

(let ((config (make-config-parser)))
  (config-read-string config test-ini-simple)
  (config-add-section! config "server")
  (check (config-has-section? config "server") => #t)
  (config-set! config "server" "name" "web")
  (check (config-get config "server" "name") => "web")
) ;let

;; ==== 17. config-remove-section!: 删除 section ====

(let ((config (make-config-parser)))
  (config-read-string config test-ini-simple)
  (config-remove-section! config "database")
  (check (config-has-section? config "database") => #f)
  (check (config-sections config) => '())
) ;let

;; ==== 18. config-remove-option!: 删除 option ====

(let ((config (make-config-parser)))
  (config-read-string config test-ini-simple)
  (config-remove-option! config "database" "host")
  (check (config-has-option? config "database" "host") => #f)
  (check (config-has-option? config "database" "port") => #t)
) ;let

;; ==== 19. config-write: 写出到字符串 ====

(let* ((config (make-config-parser))
       (_ (config-read-string config test-ini-simple))
       (result (call-with-output-string (lambda (port) (config-write config port))))
      ) ;
  (check (string-contains result "[database]") => #t)
  (check (string-contains result "host = localhost") => #t)
  (check (string-contains result "port = 5432") => #t)
) ;let*

;; ==== 20. 注释和空行被忽略 ====

(let ((config (make-config-parser)))
  (config-read-string config
    (&- #""
      ; comment

      [section]
      ; another comment
      key=val
      ""
    ) ;&-
  ) ;config-read-string
  (check (config-sections config) => '("section"))
  (check (config-get config "section" "key") => "val")
) ;let

;; ==== 21. 冒号分隔符 ====

(let ((config (make-config-parser)))
  (config-read-string config
    (&- #""
      [section]
      key: value
      ""
    ) ;&-
  ) ;config-read-string
  (check (config-get config "section" "key") => "value")
) ;let

;; ==== 22. getboolean 支持多种真/假值 ====

(let ((config (make-config-parser)))
  (config-read-string config
    (&- #""
      [section]
      a=yes
      b=true
      c=on
      d=1
      e=no
      f=false
      g=off
      h=0
      ""
    ) ;&-
  ) ;config-read-string
  (check (config-get-boolean config "section" "a") => #t)
  (check (config-get-boolean config "section" "b") => #t)
  (check (config-get-boolean config "section" "c") => #t)
  (check (config-get-boolean config "section" "d") => #t)
  (check (config-get-boolean config "section" "e") => #f)
  (check (config-get-boolean config "section" "f") => #f)
  (check (config-get-boolean config "section" "g") => #f)
  (check (config-get-boolean config "section" "h") => #f)
) ;let

;; ==== 23. 多个 = 只有第一个是分隔符 ====

(let ((config (make-config-parser)))
  (config-read-string config
    (&- #""
      [section]
      url=http://example.com?key=val
      ""
    ) ;&-
  ) ;config-read-string
  (check (config-get config "section" "url") => "http://example.com?key=val")
) ;let

;; ==== 24. config-get: section 不存在时报错 ====

(check-catch 'config-error
  (let ((config (make-config-parser)))
    (config-get config "missing" "key")
  ) ;let
) ;check-catch

;; ==== 25. config-get: option 不存在时报错 ====

(check-catch 'config-error
  (let ((config (make-config-parser)))
    (config-read-string config test-ini-simple)
    (config-get config "database" "missing")
  ) ;let
) ;check-catch

;; ==== 26. keys 不区分大小写 ====

(let ((config (make-config-parser)))
  (config-read-string config
    (&- #""
      [Section]
      MyKey=value
      ""
    ) ;&-
  ) ;config-read-string
  (check (config-get config "Section" "mykey") => "value")
  (check (config-get config "Section" "MYKEY") => "value")
  (check (config-get config "Section" "MyKey") => "value")
) ;let

;; ==== 27. config-write: 包含 DEFAULT 的输出 ====

(let* ((config (make-config-parser))
       (_ (config-read-string config
            (&- #""
              [DEFAULT]
              key=val
              [section]
              other=data
              ""
            ) ;&-
          ) ;config-read-string
       ) ;_
       (result (call-with-output-string (lambda (port) (config-write config port))))
      ) ;
  (check (string-contains result "[DEFAULT]") => #t)
  (check (string-contains result "key = val") => #t)
  (check (string-contains result "other = data") => #t)
) ;let*

;; ==== 28. Python configparser 官方 Quick Start 示例 ====

(let ((config (make-config-parser)))
  (config-read-string config test-ini)
  ;; sections() 不包含 DEFAULT
  (check (config-sections config)
    =>
    '("forge.example" "topsecret.server.example")
  ) ;check
  ;; has_section
  (check (config-has-section? config "forge.example") => #t)
  (check (config-has-section? config "python.org") => #f)
  ;; get
  (check (config-get config "forge.example" "user") => "hg")
  (check (config-get config "DEFAULT" "compression") => "yes")
  ;; DEFAULT inheritance
  (check (config-get config "forge.example" "compression") => "yes")
  ;; getint
  (check (config-get-int config "topsecret.server.example" "port") => 50022)
  ;; getboolean
  (check (config-get-boolean config "topsecret.server.example" "forwardx11")
    =>
    #f
  ) ;check
  (check (config-get-boolean config "forge.example" "forwardx11") => #t)
) ;let

(check-report)
