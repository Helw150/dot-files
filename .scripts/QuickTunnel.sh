#!/bin/bash
ssh -f $1@$2 -L $3:$2:$3 -N &
