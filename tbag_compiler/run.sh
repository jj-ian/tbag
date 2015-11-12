#!/bin/bash

./tbag < $1
javac $2
java hello_world