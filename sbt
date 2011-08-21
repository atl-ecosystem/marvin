#!/bin/bash

java $SBT_OPTS -Dfile.encoding=UTF-8 -Xss4M -XX:+UseCompressedOops -Xmx2824M -XX:MaxPermSize=256M -XX:NewSize=128M -XX:NewRatio=3 -jar \
                `dirname $0`/sbt-launch-0.10.1.jar "$@"
