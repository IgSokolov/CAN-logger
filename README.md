`CAN-logger` is a CAN bus monitor with advanced tuning features. This software is intended to be used for debugging of CAN buses in a laboratory environment.

## Introduction
TBD

## How it works
1. we access CAN bus via [SocketCAN](https://www.kernel.org/doc/html/latest/networking/can.html) (using FFI from [fsocket](https://github.com/fjames86/fsocket) library)
2. we use a simple configuration file to describe the structure of a CAN frame payload
3. we send decoded data to a monitor written with [CLX](https://sharplispers.github.io/clx/)

## How to build
Go to the directory with sources and execute `make`

## How to test
TBD

## How to use
```
./can-logger -p "path-to-config-file" -c "name-of-can-interface"
```
## Tested
SBCL 2.1.9

## Contribution:
TBD
