#!/usr/bin/env python3
"""Minimal Gemini-compatible stub for task 047 blueprint E2E tests.

Scenarios are selected by base-URL path:
  /blueprint-plan  -> assistant returns a structured JSON plan
  /blueprint-next  -> first turn calls blueprint_update(1.1, completed),
                      second turn returns a text summary
  otherwise        -> plain text
"""
import http.server
import json
import socketserver
import sys

PLAN = {
    "objective": "Add two numbers",
    "phases": [
        {
            "id": "1",
            "title": "Foundations",
            "description": "core building blocks",
            "tasks": [
                {
                    "id": "1.1",
                    "title": "Add helper",
                    "description": "implement the add helper",
                    "acceptance_criteria": ["add(1, 2) is 3"],
                    "file_targets": ["src/add.py"],
                    "dependencies": [],
                },
                {
                    "id": "1.2",
                    "title": "Add tests",
                    "description": "cover the helper with tests",
                    "acceptance_criteria": ["tests pass"],
                    "file_targets": ["tests/test_add.py"],
                    "dependencies": ["1.1"],
                },
            ],
            "verification_gates": [
                {"type": "custom_command", "description": "Run tests", "command": "true"},
                {"type": "user_approval", "description": "Review the helper"},
            ],
        },
        {
            "id": "2",
            "title": "Integration",
            "description": "wire it into the app",
            "tasks": [
                {
                    "id": "2.1",
                    "title": "Wire call site",
                    "description": "call the helper",
                    "acceptance_criteria": ["app uses helper"],
                    "file_targets": ["src/app.py"],
                    "dependencies": ["1.2"],
                },
                {
                    "id": "2.2",
                    "title": "Document",
                    "description": "document the behavior",
                    "acceptance_criteria": ["docs updated"],
                    "file_targets": ["README.md"],
                    "dependencies": ["2.1"],
                },
            ],
            "verification_gates": [
                {"type": "custom_command", "description": "Run gates", "command": "true"}
            ],
        },
    ],
}


class Handler(http.server.BaseHTTPRequestHandler):
    def do_POST(self):
        length = int(self.headers.get("content-length", 0))
        body = self.rfile.read(length) if length > 0 else b"{}"
        try:
            req = json.loads(body.decode("utf-8"))
        except Exception:
            req = {}

        path = self.path
        if "/blueprint-plan" in path:
            parts = [{"text": json.dumps(PLAN)}]
        elif "/blueprint-next" in path:
            has_function_result = False
            for content in req.get("contents", []):
                for part in content.get("parts", []):
                    if "functionResponse" in part:
                        has_function_result = True
            system_instruction = req.get("systemInstruction", "")
            if isinstance(system_instruction, dict):
                system_text = " ".join(
                    p.get("text", "")
                    for p in system_instruction.get("parts", [])
                    if isinstance(p, dict)
                )
            else:
                system_text = str(system_instruction)
            if "## Active Blueprint" not in system_text:
                parts = [{"text": "MISSING_INJECTION"}]
            elif has_function_result:
                parts = [{"text": "Task done."}]
            else:
                parts = [
                    {
                        "functionCall": {
                            "name": "blueprint_update",
                            "args": {"task_id": "1.1", "status": "completed"},
                            "id": "bp_call_1",
                        }
                    }
                ]
        else:
            parts = [{"text": "Hello from blueprint stub!"}]

        resp = {
            "candidates": [
                {
                    "content": {"role": "model", "parts": parts},
                    "finishReason": "STOP",
                }
            ],
            "usageMetadata": {
                "promptTokenCount": 20,
                "candidatesTokenCount": 10,
                "totalTokenCount": 30,
            },
        }

        if "alt=sse" in path or ":streamGenerateContent" in path:
            self.send_response(200)
            self.send_header("Content-Type", "text/event-stream")
            self.send_header("Cache-Control", "no-cache")
            self.end_headers()
            self.wfile.write(f"data: {json.dumps(resp)}\n\n".encode("utf-8"))
            self.wfile.flush()
            return

        self.send_response(200)
        self.send_header("Content-Type", "application/json")
        self.end_headers()
        self.wfile.write(json.dumps(resp).encode("utf-8"))

    def log_message(self, fmt, *args):
        pass


class Server(socketserver.ThreadingMixIn, socketserver.TCPServer):
    daemon_threads = True


def main():
    httpd = Server(("127.0.0.1", 0), Handler)
    sys.stdout.write(f"PORT:{httpd.server_address[1]}\n")
    sys.stdout.flush()
    httpd.serve_forever()


if __name__ == "__main__":
    main()
