#!/bin/bash

clear
sleep 3

echo -e "Here is a quote of the day:\n\n"
qod=`curl -s http://quotes.rest/qod.json | jq -r .contents.quotes[0].quote`
echo "  " $qod
echo ""
