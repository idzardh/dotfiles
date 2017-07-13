#!/bin/zsh

function getdefaultsinkname() {
	pacmd stat | awk -F": " '/^Default sink name: /{print $2}'
}

function getdefaultsinkvol() {
	pacmd list-sinks |
		awk '/^\s+name: /{indefault = $2 == "<'$(getdefaultsinkname)'>"}
				/^\s+volume: / && indefault {print $5; exit}' |
		awk -F"%" '{print $1}'
}

function addIcon()
{
	local s=$(getdefaultsinkvol)
	if (( $s == 0 )); then
		s="<fn=1></fn> $s"
	elif (( $s > 40 )); then
		s="<fn=1></fn> $s"
	else
		s="<fn=1></fn> $s"
	fi
	printf "%s" "$s"
}

addIcon
