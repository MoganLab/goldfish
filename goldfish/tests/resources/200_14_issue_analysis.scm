;; 分析文件特殊处理的测试用例
(test1 `(a b c)) ; 基础情况
(test2 `(#_macro (x))) ; 特殊宏符号
(test3 "AS IS" AND ANY);
(test4 `(((#_funclet 'case*) 'case*-helper) x y (#_curlet)))))
(test5 #(包括 #\(空间的 U盘 U期 )测试))