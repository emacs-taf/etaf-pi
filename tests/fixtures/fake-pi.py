#!/usr/bin/env python3
"""Offline JSONL peer. Replays a captured Pi response and simulates failures."""
import json
import sys
import threading
import time
from pathlib import Path

lock = threading.Lock()
messages = []
model = {'id': 'offline', 'provider': 'fixture', 'contextWindow': 1000000}
level = 'off'
active = False
aborted = threading.Event()
session = '/tmp/pi-fixture.jsonl'
queued = []
fixture = [json.loads(line) for line in Path(__file__).with_name('pi-stream.jsonl').read_text().splitlines()]


def emit(record):
    wire = (json.dumps(record, ensure_ascii=False) + '\r\n').encode()
    with lock:
        # Deliberately split inside UTF-8 and JSON; LF alone delimits records.
        for start in range(0, len(wire), 37):
            sys.stdout.buffer.write(wire[start:start+37])
            sys.stdout.buffer.flush()


def response(request, data=None, success=True, error=None):
    record = dict(type='response', id=request.get('id'), command=request['type'], success=success)
    if data is not None:
        record['data'] = data
    if error:
        record['error'] = error
    emit(record)


def stream(request):
    global active
    active = True
    aborted.clear()
    if request['message'] == '延迟接收':
        time.sleep(.2)
    response(request)
    for record in fixture:
        if aborted.is_set():
            break
        if record['type'] in ('response', 'agent_settled', 'agent_end'):
            continue
        if record['type'] in ('message_start', 'message_end'):
            record = json.loads(json.dumps(record))
            if record['message']['role'] == 'user':
                record['message']['content'] = request['message']
            if record['type'] == 'message_end':
                if record['message']['role'] == 'assistant' and '排版' in request['message']:
                    record['message']['content'] = [dict(type='text', text=(
                        '## 把注意力留给任务\n'
                        '我会先检查项目结构，再把改动拆成可以验证的小步。'
                        '你可以随时补充要求，不必等这轮结束。\n\n'
                        '**输入始终在手边**\n'
                        '- 回车换行，发送后继续编辑下一条。\n'
                        '- 失败不会清空草稿，确认不会自动放行。\n\n'
                        '```elisp\n(etaf-pi-open)\n;; 从当前项目开始一段对话\n```\n\n'
                        '检查结果保留在工具面板中，可随时查看完整输出。'))]
                messages.append(record['message'])
        emit(record)
        time.sleep(.006)
    tool = dict(role='toolResult', toolCallId='call-fixture', toolName='read',
                content=[dict(type='text', text='fixture.txt\n第一行\nsecond\u2028third')], isError=False)
    emit(dict(type='tool_execution_start', toolCallId='call-fixture', toolName='read', args={'path': 'fixture.txt'}))
    emit(dict(type='tool_execution_end', toolCallId='call-fixture', toolName='read', result={'content': tool['content']}, isError=False))
    messages.append(tool)
    emit(dict(type='message_end', message=tool))
    active = False
    emit(dict(type='agent_end', messages=[], willRetry=False))
    emit(dict(type='agent_settled'))


for line in sys.stdin.buffer:
    request = json.loads(line)
    kind = request['type']
    if kind == 'extension_ui_response':
        emit(dict(type='notify_test', answer=request))
    elif kind == 'get_state':
        response(request, dict(model=model, thinkingLevel=level, isStreaming=active,
                               isCompacting=False, sessionFile=session, sessionId=session))
    elif kind == 'get_messages':
        response(request, dict(messages=messages))
    elif kind == 'get_session_stats':
        response(request, dict(cost=0.0001, contextUsage=dict(tokens=300, contextWindow=1000000, percent=.03)))
    elif kind == 'get_available_models':
        response(request, dict(models=[model]))
    elif kind == 'get_available_thinking_levels':
        response(request, dict(levels=['off', 'high']))
    elif kind == 'get_commands':
        response(request, dict(commands=[dict(name='fixture-dialog', source='extension', description='Offline permission dialog')]))
    elif kind == 'prompt':
        if request['message'] == '/fixture-dialog':
            response(request)
            emit(dict(type='extension_ui_request', id='dialog-fixture', method='confirm', title='是否允许测试操作？', message='仅用于离线测试'))
        elif request['message'] == '拒绝发送':
            response(request, success=False, error='测试：暂时无法接收，文本已保留')
        elif active:
            queued.append(request['message'])
            response(request)
            emit(dict(type='queue_update', steering=[], followUp=queued))
        else:
            threading.Thread(target=stream, args=(request,), daemon=True).start()
    elif kind == 'clear_queue':
        response(request, dict(steering=[], followUp=queued[:]))
        queued.clear()
        emit(dict(type='queue_update', steering=[], followUp=[]))
    elif kind == 'abort':
        aborted.set()
        active = False
        response(request)
        emit(dict(type='agent_settled'))
    elif kind == 'set_model':
        if request['modelId'] == 'missing':
            response(request, success=False, error='Model not found')
        else:
            model = dict(id=request['modelId'], provider=request['provider'], contextWindow=1000000)
            response(request, model)
    elif kind == 'set_thinking_level':
        level = request['level']
        response(request)
    elif kind in ('new_session', 'switch_session'):
        if request.get('sessionPath') == '/tmp/cancelled.jsonl':
            response(request, dict(cancelled=True))
        else:
            session = request.get('sessionPath', '/tmp/pi-new.jsonl')
            messages.clear()
            response(request, dict(cancelled=False))
    elif kind == 'compact':
        response(request, {})
    elif kind == 'hang':
        pass
    elif kind == 'bad_json':
        with lock:
            sys.stdout.write('{invalid json}\n')
            sys.stdout.flush()
    elif kind == 'exit_now':
        sys.exit(7)
    elif kind == 'echo':
        response(request, request)
    elif kind == 'delay':
        threading.Timer(.08, lambda r=request: response(r, r)).start()
    else:
        response(request, success=False, error='Unknown command: ' + kind)
