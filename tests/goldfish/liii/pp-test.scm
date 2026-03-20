;
; Copyright (C) 2024 The Goldfish Scheme Authors
;
; Licensed under the Apache License, Version 2.0 (the "License");
; you may not use this file except in compliance with the License.
; You may obtain a copy of the License at
;
; http://www.apache.org/licenses/LICENSE-2.0
;
; Unless required by applicable law or agreed to in writing, software
; distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
; WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
; License for the specific language governing permissions and limitations
; under the License.
;

(import (liii pp)
        (liii check)
) ;import

(check-set-mode! 'report-failed)

(check (pp-parse "") => "")
(check (pp-parse "hello world") => "hello world")
(check (pp-parse "\n") => "\n")
(check (pp-parse "\n\n\n") => "\n(*PP_NEWLINE* 3)\n")
(check (pp-parse "line1\n\nline3") => "line1\n(*PP_NEWLINE* 2)\nline3")
(check (pp-parse "line1\n\n\nline4") => "line1\n(*PP_NEWLINE* 3)\nline4")
(check (pp-parse "line1\n  \nline3") => "line1\n  \nline3")  ; 只包含空白的行
(check (pp-parse "  text  \n  \n  text  ") => "  text  \n  \n  text  ")  ; 保持空格

; 单行注释测试 - 以;开头的行视为单行注释，有前置空格
(check (pp-parse "; this is a single line comment") => "(*PP_SINGLE_COMMENT* \"this is a single line comment\")")
(check (pp-parse "; comment with () parenthesis") => "(*PP_SINGLE_COMMENT* \"comment with () parenthesis\")")

; 测试单行注释边界情况
(check (pp-parse ";") => "(*PP_SINGLE_COMMENT* \"\")")
(check (pp-parse ";comment without space") => "(*PP_SINGLE_COMMENT* \"comment without space\")")
(check (pp-parse "; comment with leading space") => "(*PP_SINGLE_COMMENT* \"comment with leading space\")")

; 测试 tab 字符作为空白的情况
; (check (pp-parse "\t;tab before semicolon") => "(*PP_SINGLE_COMMENT* \"tab before semicolon\")")
; (check (pp-parse " \t ;mixed whitespace before semicolon") => "(*PP_SINGLE_COMMENT* \"mixed whitespace before semicolon\")")

; 测试空字符串和仅空白字符的情况
(check (pp-parse "") => "")
(check (pp-parse "   ") => "   ")
(check (pp-parse "\t\t") => "\t\t")

; 多行注释测试

; 测试基本的多行注释格式
(check (pp-parse "#|x\n|#\n(define x 1)")
       => "(*PP_MULTI_COMMENT* \"x\" \"\")\n(define x 1)"
) ;check
       
; 测试包含空格的格式  
(check (pp-parse "#|\n  x  \n|#\n(define x 1)")
       => "(*PP_MULTI_COMMENT* \"\" \"  x  \" \"\")\n(define x 1)"
) ;check

; 测试多行内容
(check (pp-parse "#|x\nsecond\n|#\n(define x 1)")
       => "(*PP_MULTI_COMMENT* \"x\" \"second\" \"\")\n(define x 1)"
) ;check

; 测试空白内容的多行注释
(check (pp-parse "#|\n\n|#") => "(*PP_MULTI_COMMENT* \"\" \"\" \"\")")

; 测试 base64 文件中的实际多行注释格式
(check (pp-parse "#|\nbase64-encode\n将字符串编码为 Base64 格式。\n|#")
       => "(*PP_MULTI_COMMENT* \"\" \"base64-encode\" \"将字符串编码为 Base64 格式。\" \"\")"
) ;check

; 测试复杂多行注释（包含多个段落）
(check (pp-parse "#|\nbase64-encode\n将字符串编码为 Base64 格式。\n\n语法\n----\n(base64-encode str)\n|#")
       => "(*PP_MULTI_COMMENT* \"\" \"base64-encode\" \"将字符串编码为 Base64 格式。\" \"\" \"语法\" \"----\" \"(base64-encode str)\" \"\")"
) ;check

; 测试多行注释边界情况
(check (pp-parse "#||#") => "(*PP_MULTI_COMMENT* \"\")")
(check (pp-parse "#|single line|#") => "(*PP_MULTI_COMMENT* \"single line\")")
(check (pp-parse "#|line1\nline2\nline3|#") => "(*PP_MULTI_COMMENT* \"line1\" \"line2\" \"line3\")")

; 测试多行注释与代码混合
(check (pp-parse "#|\n注释内容\n|#\n(define test 1)")
       => "(*PP_MULTI_COMMENT* \"\" \"注释内容\" \"\")\n(define test 1)"
) ;check

; 测试多行注释中的特殊字符
(check (pp-parse "#|\n注释()[]{}\n特殊字符!@#$%^&*\n|#")
       => "(*PP_MULTI_COMMENT* \"\" \"注释()[]{}\" \"特殊字符!@#$%^&*\" \"\")"
) ;check

; pp-post 测试用例：normal -> newline 和 newline -> normal 状态转移

; 测试简单换行转换：normal 状态读取字符直到遇到换行符，然后转换到 newline 状态
(check (pp-post "hello\nworld") => "hello\nworld")

; 测试 newline -> normal 转换：从换行状态回到 normal 状态
(check (pp-post "hello\n\nworld") => "hello\n\nworld")

; 测试连续空行的转换: PP_NEWLINE 现在由 pretty-printer 处理，不再由 pp-post 处理
; 所以 pp-post 会原样输出 PP_NEWLINE 表达式
(check (pp-post "line1\n(*PP_NEWLINE* 3)\nline2") => "line1\n(*PP_NEWLINE* 3)\nline2")

(check-report)

