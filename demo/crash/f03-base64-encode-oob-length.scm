;; g_bytevector-base64-encode 同样不校验第二个参数(长度)，
;; 声明 1GB 但实际只有 4 字节 => 编码循环越界读取 => SIGSEGV
(g_bytevector-base64-encode (make-bytevector 4 65) 1073741824)
