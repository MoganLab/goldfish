;; (liii pi) 模块函数分类索引
;;
;; pi 模块提供与 pi coding agent wire 协议、配置、消息序列化与工具执行相关的核心实现。
;; 包含系统提示词组装、Gemini 与 Anthropic 消息协议往返转换、工具 schema 与执行器等。
;;
;; ==== 如何查看函数的文档和用例 ====
;;   bin/gf doc liii/pi "build-initial-prompt"
;;   bin/gf doc liii/pi "gemini-build-contents"
;;   bin/gf doc liii/pi "anthropic-build-messages"

(import (liii pi))
