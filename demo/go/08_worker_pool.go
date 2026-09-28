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

// worker 工作协程：
// 并发从 jobs 通道中抢占任务，计算翻倍后将结果送入 results 通道。
// 当 jobs 通道关闭且排空后，for-range 循环自动退出。
func worker(id int, jobs <-chan int, results chan<- int) {
	for j := range jobs {
		fmt.Printf("[worker %d] 开始处理任务: %d\n", id, j)
		time.Sleep(20 * time.Millisecond) // 模拟计算耗时
		results <- j * 2
	}
}

func main() {
	const numJobs = 5
	jobs := make(chan int, numJobs)
	results := make(chan int, numJobs)

	// 1. 启动 3 个 worker 协程组成工作池（并发限流为 3）
	for w := 1; w <= 3; w++ {
		go worker(w, jobs, results)
	}

	// 2. 发送 5 个任务，发送完毕后关闭 jobs 通道
	for j := 1; j <= numJobs; j++ {
		jobs <- j
	}
	close(jobs)

	// 3. 收集所有任务的处理结果
	for a := 1; a <= numJobs; a++ {
		fmt.Printf("收到结果: %d\n", <-results)
	}
}
