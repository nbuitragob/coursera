#!/bin/sh
exec scala "$0" "$@"
!#

scala.io.Source.fromFile(System.getProperty("user.home/") + "coursera/.scala_history").foreach(print)
