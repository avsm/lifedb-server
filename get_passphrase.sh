#!/bin/sh

/usr/bin/security find-generic-password -s LifeDB -a root -g 2>&1 | grep ^password | sed -e 's/password: \"//' -e 's/\"$//g'
