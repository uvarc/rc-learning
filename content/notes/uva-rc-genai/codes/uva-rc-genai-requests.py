import os 
import requests 
 
response = requests.post( 
    "https://open-webui.rc.virginia.edu/api/chat/completions", 
    headers={"Authorization": f"Bearer {os.environ.get('UVARC_GenAI_API')}"}, 
    json={ 
        "model": "Kimi K2.5", 
        "messages": [{"role": "user", "content": "Hello"}] 
    } 
)
