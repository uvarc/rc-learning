#!/bin/bash       
# 'echo' is fine for printing single line messages,    
# but somewhat problematic for message blocks.
# A 'cat' here document overcomes this limitation.     
cat << End-of-message
-------------------------------------
This is line 1 of the message. 
This is line 2 of the message.
This is line 3 of the message.
This is line 4 of the message.
This is the last line of the message.
-------------------------------------
End-of-message
