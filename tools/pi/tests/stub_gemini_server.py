import http.server
import socketserver
import sys
import json
import threading

class StubHandler(http.server.BaseHTTPRequestHandler):
    def do_POST(self):
        len_h = int(self.headers.get("content-length", 0))
        body = self.rfile.read(len_h) if len_h > 0 else b""
        req_json = {}
        if body:
            try:
                req_json = json.loads(body.decode("utf-8"))
            except:
                pass
        
        path = self.path
        if "alt=sse" in path or ":streamGenerateContent" in path:
            # 模拟 SSE 流式响应
            self.send_response(200)
            self.send_header("Content-Type", "text/event-stream")
            self.send_header("Cache-Control", "no-cache")
            self.end_headers()
            
            if "/stream-idle-hang" in path:
                # 模拟输出部分 chunk 后静默挂起超时
                chunk1 = {"candidates": [{"content": {"role": "model", "parts": [{"text": "Start..."}]}}]}
                self.wfile.write(f"data: {json.dumps(chunk1)}\n\n".encode("utf-8"))
                self.wfile.flush()
                import time
                time.sleep(30)
                return

            if "/stream-function-call" in path:
                chunk1 = {
                    "candidates": [{
                        "content": {
                            "role": "model",
                            "parts": [{
                                "functionCall": {
                                    "name": "bash",
                                    "args": {"command": "ls -la"},
                                    "id": "stream_call_1"
                                }
                            }]
                        },
                        "finishReason": "STOP"
                    }],
                    "usageMetadata": {"promptTokenCount": 10, "candidatesTokenCount": 5, "totalTokenCount": 15}
                }
                self.wfile.write(f"data: {json.dumps(chunk1)}\n\n".encode("utf-8"))
                self.wfile.flush()
                return

            # 默认 SSE 流式：分2个 chunk 发送
            chunk1 = {
                "candidates": [{
                    "content": {
                        "role": "model",
                        "parts": [{"text": "Hello "}]
                    }
                }]
            }
            chunk2 = {
                "candidates": [{
                    "content": {
                        "role": "model",
                        "parts": [{"text": "stream!"}]
                    },
                    "finishReason": "STOP"
                }],
                "usageMetadata": {
                    "promptTokenCount": 10,
                    "candidatesTokenCount": 6,
                    "totalTokenCount": 16
                }
            }
            self.wfile.write(f"data: {json.dumps(chunk1)}\n\n".encode("utf-8"))
            self.wfile.flush()
            self.wfile.write(f"data: {json.dumps(chunk2)}\n\n".encode("utf-8"))
            self.wfile.flush()
            return

        if "/429-then-ok" in path:
            server = self.server
            server.request_count = getattr(server, "request_count", 0) + 1
            if server.request_count < 3:
                self.send_response(429)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                self.wfile.write(b'{"error":{"code":429,"message":"Rate limit exceeded","status":"RESOURCE_EXHAUSTED"}}')
                return

        if "/error-500" in path:
            self.send_response(500)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(b'{"error":{"code":500,"message":"Internal Server Error"}}')
            return

        if "/max-tokens" in path:
            resp = {
                "candidates": [
                    {
                        "content": {"role": "model", "parts": [{"text": "Partial cut"}]},
                        "finishReason": "MAX_TOKENS"
                    }
                ],
                "usageMetadata": {
                    "promptTokenCount": 10,
                    "candidatesTokenCount": 5,
                    "totalTokenCount": 15
                }
            }
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        if "/safety" in path:
            resp = {
                "candidates": [
                    {
                        "content": {"role": "model", "parts": []},
                        "finishReason": "SAFETY"
                    }
                ],
                "usageMetadata": {
                    "promptTokenCount": 5,
                    "candidatesTokenCount": 0,
                    "totalTokenCount": 5
                }
            }
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        if "/function-call" in path:
            resp = {
                "candidates": [
                    {
                        "content": {
                            "role": "model",
                            "parts": [
                                {
                                    "functionCall": {
                                        "name": "bash",
                                        "args": {"command": "ls -la"},
                                        "id": "bash_call_1"
                                    }
                                }
                            ]
                        },
                        "finishReason": "STOP"
                    }
                ],
                "usageMetadata": {
                    "promptTokenCount": 20,
                    "candidatesTokenCount": 10,
                    "totalTokenCount": 30
                }
            }
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        if "/multi-turn" in path:
            contents = req_json.get("contents", [])
            fn_responses = []
            for c in contents:
                for p in c.get("parts", []):
                    if "functionResponse" in p:
                        fn_responses.append(p["functionResponse"])
            if len(fn_responses) == 0:
                resp = {
                    "candidates": [
                        {
                            "content": {
                                "role": "model",
                                "parts": [
                                    {
                                        "functionCall": {
                                            "name": "bash",
                                            "args": {"command": "echo 'step 1'"},
                                            "id": "call_turn_1"
                                        }
                                    }
                                ]
                            },
                            "finishReason": "STOP"
                        }
                    ],
                    "usageMetadata": {"promptTokenCount": 20, "candidatesTokenCount": 10, "totalTokenCount": 30}
                }
            elif len(fn_responses) == 1:
                resp = {
                    "candidates": [
                        {
                            "content": {
                                "role": "model",
                                "parts": [
                                    {
                                        "functionCall": {
                                            "name": "bash",
                                            "args": {"command": "echo 'step 2'"},
                                            "id": "call_turn_2"
                                        }
                                    }
                                ]
                            },
                            "finishReason": "STOP"
                        }
                    ],
                    "usageMetadata": {"promptTokenCount": 40, "candidatesTokenCount": 10, "totalTokenCount": 50}
                }
            else:
                resp = {
                    "candidates": [
                        {
                            "content": {
                                "role": "model",
                                "parts": [{"text": "Multi-turn tool completed successfully!"}]
                            },
                            "finishReason": "STOP"
                        }
                    ],
                    "usageMetadata": {"promptTokenCount": 60, "candidatesTokenCount": 10, "totalTokenCount": 70}
                }
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        if "/check-project-context" in path:
            sys_inst = req_json.get("systemInstruction", "")
            if "project_context" in sys_inst and "CLAUDE.md" in sys_inst:
                reply = "Project context verified: CLAUDE.md is present"
            else:
                reply = "Project context missing"
            resp = {
                "candidates": [
                    {
                        "content": {"role": "model", "parts": [{"text": reply}]},
                        "finishReason": "STOP"
                    }
                ],
                "usageMetadata": {"promptTokenCount": 20, "candidatesTokenCount": 10, "totalTokenCount": 30}
            }
            self.send_response(200)
            self.send_header("Content-Type", "application/json")
            self.end_headers()
            self.wfile.write(json.dumps(resp).encode("utf-8"))
            return

        # default echo ok
        resp = {
            "candidates": [
                {
                    "content": {
                        "role": "model",
                        "parts": [{"text": "Hello from stub!"}]
                    },
                    "finishReason": "STOP"
                }
            ],
            "usageMetadata": {
                "promptTokenCount": 15,
                "candidatesTokenCount": 5,
                "totalTokenCount": 20
            },
            "echo": {
                "apiKey": self.headers.get("x-goog-api-key"),
                "body": req_json
            }
        }
        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(json.dumps(resp).encode("utf-8"))

    def log_message(self, format, *args):
        pass

def main():
    httpd = socketserver.TCPServer(("127.0.0.1", 0), StubHandler)
    port = httpd.server_address[1]
    # Print port to stdout and flush so child process waiter can read it
    sys.stdout.write(f"PORT:{port}\n")
    sys.stdout.flush()
    httpd.serve_forever()

if __name__ == "__main__":
    main()
