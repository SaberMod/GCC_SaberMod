#!/usr/bin/python
#
# Copyright (C) 2013 Free Software Foundation, Inc.
#
# This script is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3, or (at your option)
# any later version.

# This script computes and outputs the percentage of profile that has
# been used to annotate the AutoFDO optimized binary.

import subprocess
import sys

if len(sys.argv) != 2:
  print "Usage: " + sys.argv[0] + " <path_to_autofdo_optimized_binary>"
  exit(1)

args = ["readelf", "-p", ".gnu.switches.text.annotation", sys.argv[1]]
output, _ = subprocess.Popen(args, stdout=subprocess.PIPE).communicate()

profile_map = {}

for l in output.split('\n'):
  parts = l.split()
  if len(parts) != 3:
    continue;
  words = parts[2].split(':')
  if len(words) != 3:
    continue;
  function = words[0]
  total_count = int(words[1])
  annotated_count = int(words[2])
  if function not in profile_map:
    profile_map[function] = [total_count, annotated_count]
  elif annotated_count > profile_map[function][1]:
    profile_map[function][1] = annotated_count

total_sum = 0
annotated_sum = 0
for function in profile_map:
  total_sum += profile_map[function][0]
  annotated_sum += profile_map[function][1]

print float(annotated_sum) / total_sum
