(import (liii check))
(import (liii cmark inline util))

(check-set-mode! 'report-failed)

;; find-next-special
;; 找到下一个 inline 特殊字符的位置（或 #f）。特殊字符含强调/链接/码/转义/自动链接/emoji 等。
;;
;; 语法
;; ----
;; (find-next-special str pos)
;;
;; 描述
;; ----
;; 返回 [pos, 结束) 内第一个特殊字符的下标；无则 #f。
;; 本测试重点覆盖 1032 性能优化引入的 `find-next-special-else-bound` 整体跳过逻辑：
;; 连续 email-candidate 字符串中不含 '@' / 'h'（无法构成 autolink）时，须正确跳过
;; 且不误判、不漏掉真正的特殊字符。

;; 普通英文/中文文本：无任何特殊字符 → #f
(check (find-next-special "hello world" 0) => #f)
(check (find-next-special "普通中文正文没有特殊字符" 0) => #f)

;; 连续 email-candidate 长串（无 @ / h，触发跳过路径）：跳到串尾 → #f
(check (find-next-special "aaaaaaaaaa" 0) => #f)
(check (find-next-special "aaa....bbb" 0) => #f)
(check (find-next-special "abcdefghijk" 0) => #f)

;; 含 '@' 的 email 候选：应返回 email 起始（local 起点 = 0）
(check (find-next-special "foo@bar.com" 0) => 0)
(check (find-next-special "xxx foo@bar.com" 0) => 4)
(check (find-next-special "xx@yy" 0) => 0)

;; 含 'h' 的 URL 前缀：应返回 http 的 'h'。注意 URL 需满足前置边界（前一个字符不是字母数字）。
(check (find-next-special "https://a.b" 0) => 0)
(check (find-next-special "see https://a.b now" 0) => 4)
;; 无边界（'https' 前紧邻字母 'a'）→ 不识别为 URL，且串内无其它特殊字符 → #f
(check (find-next-special "aaahttps://x.com" 0) => #f)

;; 含强调字符：应返回其下标（不被 email 跳过逻辑吞掉）
(check (find-next-special "aaaa*bbbb" 0) => 4)
(check (find-next-special "aaaa_bbbb" 0) => 4)
(check (find-next-special "aaa```bbb" 0) => 3)
(check (find-next-special "aaa~bbb" 0) => 3)
(check (find-next-special "aaa!bbb" 0) => 3)

;; 反斜杠转义
(check (find-next-special "aaa\\bbb" 0) => 3)

;; 链接 / 换行
(check (find-next-special "aaa[bbb" 0) => 3)
(check (find-next-special "aaa\nbbb" 0) => 3)

;; 从指定 pos 后搜索
(check (find-next-special "aaaa*bbbb" 5) => #f)

;; 边界场景：多 @、空 local、email 紧邻边界、URL 后接文本
(check (find-next-special "a@b" 0) => 0)
(check (find-next-special "x a@b" 0) => 2)
(check (find-next-special "foo@" 0) => #f)
(check (find-next-special "%+~foo@bar.b" 0) => 0)
(check (find-next-special "xx yy=zz@mail.com tail" 0) => 3)
(check (find-next-special "见 https://a.b 见" 0) => 4)
