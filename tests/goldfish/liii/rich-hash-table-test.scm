;
; Copyright (C) 2025 The Goldfish Scheme Authors
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

(import (liii check)
        (scheme base)
        (liii rich-hash-table)
        (liii lang)
        (liii error)
) ;import

(check-set-mode! 'report-failed)


#|
rich-hash-table%size
获取rich-hash-table中键值对的数量。

语法
----
(rich-hash-table-instance :size)

参数
----
无参数。

返回值
-----
返回一个整数，表示rich-hash-table中键值对的数量。

说明
----
返回当前rich-hash-table实例中包含的键值对数量。对于空的rich-hash-table，返回0。
该操作不会修改rich-hash-table的内容。

边界条件
--------
- 空rich-hash-table：返回0
- 包含元素的rich-hash-table：返回实际元素数量
- 支持链式调用：可与其他rich-hash-table方法组合使用

性能特征
--------
- 时间复杂度：O(1)，直接获取内部hash-table的大小
- 空间复杂度：O(1)，不创建额外数据结构

兼容性
------
- 与所有rich-hash-table实例方法兼容
- 支持链式调用模式
|#

;; 基本测试
(check ((rich-hash-table :empty) :size) => 0)

(check ($ (hash-table 'a 1 'b 2 'c 3) :size) => 3)

(check-report)
