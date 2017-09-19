#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar

# wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

# launch polybar
if [ "$(hostname)" = "pcgish" ]
then
  polybar desktop &
else
  polybar laptop &
fi

echo "Bars launched..."
