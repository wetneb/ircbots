#!/bin/sh

cat fifo-devroom | ./irctk hurlbot@localhost '#devroom' | ./bot > fifo-devroom 2> log-devroom


