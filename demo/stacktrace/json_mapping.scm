;; json_mapping.scm: JSON 反序列化与嵌套哈希表数据提取中的调用栈回溯演示

(import (scheme base) (liii json) (liii hash-table))

;; 步骤 1：从嵌套哈希表中提取服务端口

(define (extract-port server-config)
  (let ((port (hash-table-ref server-config "port")))
    (cons "port" (+ port 1))
  ) ;let
) ;define

;; 步骤 2：解析整个服务配置

(define (load-service-config json-str)
  (let ((data (string->json json-str)))
    (cons "service" (extract-port data))
  ) ;let
) ;define

;; 入口：JSON 中只有 "host"，缺少必需的 "port" 键，触发深层 hash-table-ref key-error
(load-service-config "{\"host\": \"localhost\"}")
