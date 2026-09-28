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
	"context"
	"fmt"
	"time"
)

// worker 后台任务协程：
// 从 jobs 通道读取数据并翻倍处理写入 out 通道；
// 同时监听 ctx.Done() 取消信号，一旦收到信号立即退出并通过 defer close(out) 通知下游。
func worker(ctx context.Context, jobs <-chan int, out chan<- int) {
	defer close(out)
	for {
		select {
		case <-ctx.Done():
			fmt.Println("worker exit by ctx")
			return
		case j := <-jobs:
			out <- j * 2
		}
	}
}

func main() {
	// jobs: 缓冲容量为 3，发送 3 个任务后故意不关闭，演示在任务流未显式结束时靠 ctx 取消优雅退出
	jobs := make(chan int, 3)
	// out: 缓冲容量为 3，避免 worker 发送结果时阻塞
	out := make(chan int, 3)

	// 创建可取消的上下文 context
	ctx, cancel := context.WithCancel(context.Background())
	go worker(ctx, jobs, out)

	// 向 jobs 发送 3 个任务
	jobs <- 1
	jobs <- 2
	jobs <- 3

	// 等待 50 毫秒让 worker 处理完任务，随后主动发起取消通知
	time.Sleep(50 * time.Millisecond)
	cancel()

	// 主协程等待 out 关闭并消费所有产出（worker 退出时 defer 关闭 out）
	for v := range out {
		fmt.Println("result:", v)
	}
}
