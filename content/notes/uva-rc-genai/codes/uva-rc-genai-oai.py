import os
import openai

client = openai.OpenAI(
    base_url="https://open-webui.rc.virginia.edu/api/",
    api_key=os.environ.get("UVARC_GenAI_API")
)

response = client.chat.completions.create(
    model="Kimi K2.5",
    messages=[{"role": "user", "content": "Hello"}],
    stream=True
)

# Handle streaming response
full_text = ""
for chunk in response:
    if chunk.choices[0].delta.content:
        full_text += chunk.choices[0].delta.content

print(full_text)
