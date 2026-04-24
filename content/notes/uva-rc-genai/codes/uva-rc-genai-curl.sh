export  UVARC_GenAI_API ="<yourAPIkey>" 
curl -X POST "https://open-webui.rc.virginia.edu/api/chat/completions" \ 
     -H "Authorization: Bearer $UVARC_GenAI_API" \ 
     -H "Content-Type: application/json" \ 
     -d '{"model": "Kimi K2.5", "messages": [{"role": "user", "content": "Hello"}]}'
