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

import (
	"fmt"
)

// 展示 Go select 规范规定的随机公平性（Uniform Pseudo-Random Selection）：
// 当多个 case 同时处于就绪状态时，select 会以伪随机均匀概率选择其中一个执行，
// 避免按书写顺序线性扫描导致的后序分支饥饿现象。
func main() {
	// chA 和 chB 均为容量为 1 的缓冲通道，且初始都塞满数据（均处于可读就绪状态）
	chA := make(chan string, 1)
	chB := make(chan string, 1)
	chA <- "Channel-A"
	chB <- "Channel-B"

	countA := 0
	countB := 0
	rounds := 20

	fmt.Println("=== 两个分支同时常态就绪时的 20 次 select 效果 (Go) ===")
	for i := 1; i <= rounds; i++ {
		select {
		case v := <-chA:
			countA++
			fmt.Printf("Round %2d: 选中了 %s\n", i, v)
			chA <- "Channel-A" // 重新补满，保持就绪
		case v := <-chB:
			countB++
			fmt.Printf("Round %2d: 选中了 %s\n", i, v)
			chB <- "Channel-B" // 重新补满，保持就绪
		}
	}

	fmt.Printf("\n统计结果: Channel-A 选中 %d 次, Channel-B 选中 %d 次\n", countA, countB)
}
