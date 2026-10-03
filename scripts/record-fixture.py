#!/usr/bin/env python3
"""Explicit, opt-in recording of the installed Pi's RPC wire format."""
import argparse
import json
import os
import selectors
import subprocess
import tempfile
import time
from pathlib import Path


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--with-model', action='store_true', required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    # Pi's model-proxy extension is intentionally absent. Use direct routing for
    # the explicitly selected DeepSeek endpoint, not an inherited dead proxy.
    env = os.environ.copy()
    for key in ('HTTP_PROXY', 'HTTPS_PROXY', 'ALL_PROXY', 'http_proxy', 'https_proxy', 'all_proxy'):
        env.pop(key, None)
    env.update(NO_PROXY='*', no_proxy='*')
    with tempfile.TemporaryDirectory() as cwd:
        process = subprocess.Popen(
            ['pi', '--mode', 'rpc', '--no-session', '--no-extensions', '--no-skills',
             '--no-context-files', '--no-tools', '--thinking', 'off',
             '--model', 'deepseek/deepseek-flash'], cwd=cwd, env=env,
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.DEVNULL)
        records = []
        selector = selectors.DefaultSelector()
        selector.register(process.stdout, selectors.EVENT_READ)
        try:
            process.stdin.write(b'{"id":"fixture","type":"prompt","message":"Reply with three short Chinese sentences about Emacs. No tools."}\n')
            process.stdin.flush()
            tail = b''
            deadline = time.monotonic() + 90
            settled = False
            while not settled and time.monotonic() < deadline:
                for key, _ in selector.select(timeout=1):
                    chunk = os.read(key.fd, 65536)
                    if not chunk:
                        raise RuntimeError('Pi exited before agent_settled')
                    tail += chunk
                    while b'\n' in tail:
                        line, tail = tail.split(b'\n', 1)
                        record = json.loads(line)
                        records.append(record)
                        settled = settled or record.get('type') == 'agent_settled'
            if not settled:
                raise TimeoutError('No agent_settled within 90 seconds')
            if any(r.get('message', {}).get('stopReason') == 'error' for r in records):
                raise RuntimeError('Provider failed; fixture not written')
            args.output.parent.mkdir(parents=True, exist_ok=True)
            args.output.write_text(''.join(json.dumps(r, ensure_ascii=False) + '\n' for r in records))
            print(f'recorded={len(records)} output={args.output}', flush=True)
        finally:
            selector.close()
            process.stdin.close()
            try:
                process.wait(timeout=3)
            except subprocess.TimeoutExpired:
                process.terminate()
                try:
                    process.wait(timeout=3)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait()


if __name__ == '__main__':
    main()
