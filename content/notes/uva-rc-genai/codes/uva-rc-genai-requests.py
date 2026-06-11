import os 
import requests 
import json

with requests.post( 
    "https://open-webui.rc.virginia.edu/api/chat/completions", 
    headers={"Authorization": f"Bearer {os.environ.get('UVARC_GenAI_API')}",
            'Content-Type': 'application/json'
            }, 
    json={ 
        "model": "Kimi K2.5", 
        "messages": [{"role": "user", "content": "Hello"}] 
    } 
) as resp:
    resp.raise_for_status()

    full_text = ""

    for line in resp.iter_lines(decode_unicode=True):
        if not line:
            continue

        if line.startswith("data: "):
            data = line.removeprefix("data: ").strip()

            if data == "[DONE]":
                break

            chunk = json.loads(data)
            delta = chunk["choices"][0].get("delta", {})
            token = delta.get("content", "")

            print(token, end="", flush=True)
            full_text += token
            
