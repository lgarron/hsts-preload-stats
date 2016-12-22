#!/usr/bin/env bash

INPUT_FILE="M48-M50.txt"
SCAN_OUTPUT="./scan-output"

for SITE in $(cat "$INPUT_FILE")
do
  echo "$SITE"
  SITE_OUTPUT_FILE="$SCAN_OUTPUT/$SITE.txt"
  touch "$SITE_OUTPUT_FILE"
  http --default-scheme https -ph $SITE 2>1 >> "$SITE_OUTPUT_FILE" &
  sleep 0.2
done