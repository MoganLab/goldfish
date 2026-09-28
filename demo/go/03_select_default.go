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
	// ready: 容量为 1 的缓冲通道，预先写入一个值，使其处于可立即读取的就绪状态
	ready := make(chan string, 1)
	ready <- "ready-value"

	// notReady: 无缓冲通道且无发送者，处于未就绪状态（读取会阻塞）
	notReady := make(chan int)

	// 演示 1：多个分支中有一个分支就绪
	// select 会选择已就绪的 ready 分支执行，忽略未就绪分支和 default 分支
	select {
	case v := <-ready:
		fmt.Println("got ready:", v)
	case v := <-notReady:
		fmt.Println("got notReady:", v)
	default:
		fmt.Println("default: no case ready")
	}

	// 演示 2：所有通道分支均未就绪
	// 当所有 case 都阻塞且存在 default 分支时，select 会以非阻塞方式立即执行 default 分支
	select {
	case v := <-notReady:
		fmt.Println("unexpected:", v)
	default:
		fmt.Println("default: notReady not ready")
	}
}
