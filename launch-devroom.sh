#!/bin/sh

cat fifo-devroom | ./irctk titobot@localhost '#devroom' | ./bot > fifo-devroom 2> log-devroom


