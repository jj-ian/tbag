#!/bin/bash

./tbag < $1
javac $2
java test_hello_world