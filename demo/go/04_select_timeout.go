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

func main() {
	// ch: 无缓冲通道，且无发送者，接收操作必然阻塞
	ch := make(chan int)
	start := time.Now()

	// select 配合 time.After 实现超时控制模式（Timeout Pattern）：
	// - time.After 返回一个单向通道 <-chan Time，在指定时长后向通道发送触发时刻的时间对象 t；
	// - case t := <-time.After(...) 接收该时间对象，并通过 t.Sub(start) 记录并打印实际等待耗时。
	select {
	case v := <-ch:
		fmt.Println("got:", v)
	case t := <-time.After(100 * time.Millisecond):
		elapsed := t.Sub(start).Round(time.Millisecond)
		fmt.Println("timeout after:", elapsed)
	}
}
