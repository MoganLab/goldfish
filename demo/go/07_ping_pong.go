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
	"time"
)

// player 选手协程：从球台接球并回击
// table 通道为无缓冲通道，保证同一时刻只有一个选手持球（交会机制 Rendezvous）
func player(name string, table chan int, done chan string) {
	for hits := range table {
		fmt.Printf("[%s] 击球: %d\n", name, hits)
		time.Sleep(20 * time.Millisecond) // 模拟挥拍击球耗时
		if hits >= 6 {
			// 达到第 6 次击球，关闭球台结束比赛
			close(table)
			break
		}
		table <- hits + 1
	}
	done <- name
}

func main() {
	table := make(chan int)
	done := make(chan string, 2)

	go player("ping", table, done)
	go player("pong", table, done)

	// 裁判开球：发入第 1 球
	table <- 1

	// 等待两名选手完成比赛并离场
	<-done
	<-done
	fmt.Println("比赛圆满结束")
}
