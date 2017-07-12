#!/bin/bash

getdefaultsinkname() {
	pacmd stat | awk -F": " '/^Default sink name: /{print $2}'
}

getdefaultsinkvol() {
	pacmd list-sinks |
		awk '/^\s+name: /{indefault = $2 == "<'$(getdefaultsinkname)'>"}
				/^\s+volume: / && indefault {print $5; exit}' |
		awk -F"%" '{print $1}' | awk '{print "Vol: "$1"%"}'
}

getdefaultsinkvol
