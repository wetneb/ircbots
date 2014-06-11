#!/bin/sh

cat fifo-devroom | ./irctk hurl@localhost '#devroom' | ./bot > fifo-devroom 2> log-devroom


