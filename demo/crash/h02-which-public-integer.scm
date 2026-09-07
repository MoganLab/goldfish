;; (liii sys) 的 which 公开 API 直接透传 g_which，
;; 整数参数被当作命令名字符串 => std::string 构造自空指针 => abort
(import (liii sys))
(which 123)
