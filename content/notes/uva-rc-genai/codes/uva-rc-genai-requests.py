import os 
import requests 
import json

resp = requests.post( 
    "https://open-webui.rc.virginia.edu/api/chat/completions", 
    headers={
        "Authorization": f"Bearer {os.environ.get('UVARC_GenAI_API')}",
        "Content-Type": "application/json"
    }, 
    json={ 
        "model": "<model>", 
        "messages": [{"role": "user", "content": "Hello"}],
        "stream": False 
    }
)

resp.raise_for_status()

data = resp.json()
full_text = data["choices"][0]["message"]["content"]

print(full_text)
