import os
import openai

client = openai.OpenAI(
    base_url="https://open-webui.rc.virginia.edu/api/",
    api_key=os.environ.get("UVARC_GenAI_API")
)

response = client.chat.completions.create(
    model="<model>",
    messages=[{"role": "user", "content": "Hello"}],
    stream=False
)


print(response.choices[0].message.content)
