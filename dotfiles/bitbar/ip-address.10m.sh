#!/usr/bin/env bash

if [[ "$1" = "copy" ]]; then
  echo "$2" | pbcopy
  osascript -e "display notification \"Copied $2 to Clipboard\" with title \"🌎 IP Address\"" &>/dev/null
  exit
fi

echo '🌎'
echo '---'

ip=$(ifconfig | grep 'inet ' | awk 'FNR==2{print $2}')
myip="$(dig +short myip.opendns.com @resolver1.opendns.com)"

echo "local: $ip"
echo "remote: $myip"
echo '---'
echo "📋 copy | bash='$0' param1=copy param2='$ip' refresh=false terminal=false"
echo 'Refresh... | refresh=true'
