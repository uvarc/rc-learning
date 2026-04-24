import os 
import openai 
 
client = openai.OpenAI( 
    base_url="https://open-webui.rc.virginia.edu/api/", 
    api_key=os.environ.get("UVARC_GenAI_API") 
) 
 
response = client.chat.completions.create( 
    model="Kimi K2.5", 
    messages=[{"role": "user", "content": "Hello"}] 
) 

print(response)
