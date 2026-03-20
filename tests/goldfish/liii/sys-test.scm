(import (liii check)
        (liii sys)
) ;import

#|
argv
返回一个程序命令行参数列表

语法
----
(argv)

参数
----
无

返回值
-----
list
例如：
> bin/gf demo/demo_argv.scm
("bin/gf" "demo/demo_argv.scm")
返回一个列表，第一个表示的是命令行的命令名；第二个表示的是命令行第一个参数

功能
----
argv 用于获取命令行参数返回一个存储命令行参数的列表。


性能特征
-------
空间复杂度：O(1)：存储一个列表

|#
(check-true (list? (argv)))

#|
executable
用来返回程序可执行文件的绝对路径

语法
----
(executable)

参数
----
无

返回值
-----
string
返回Goldfish Scheme解释器的绝对路径

边界条件
--------
无

性能特征
-------
-时间复杂度：O(1)，函数只进行一次绝对路径的访问。
-空间复杂度：O(1)，函数只返回一个字符串。
|#

(check-true (string? (executable)))


(check-report)
