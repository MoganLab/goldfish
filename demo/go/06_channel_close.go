//
// Copyright (C) 2026 The Goldfish Scheme Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations
// under the License.
//

package main

import "fmt"

func main() {
	// 创建容量为 1 的缓冲通道，写入数据后立即关闭
	ch := make(chan int, 1)
	ch <- 7
	close(ch)

	// 1. 通道虽已关闭，但缓冲区中仍有残留数据，依然可以正常接收
	fmt.Println(<-ch) // 7

	// 2. 通道已关闭且缓冲区已排空：
	//    使用 comma-ok 惯用法接收，v 得到对应类型的零值 0，ok 为 false
	v, ok := <-ch
	fmt.Println(v, ok) // 0 false
}
