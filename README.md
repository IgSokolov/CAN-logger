## Introduction
`CAN-logger` is a real-time CAN bus monitor.
<p align="center">
	<img src=demo.gif width=70% height=70%>
</p>
This application reads CAN frames from a data bus, decodes their payload and plots data in real-time.

## How it works
1. we access CAN bus via [SocketCAN](https://www.kernel.org/doc/html/latest/networking/can.html) (using FFI from [fsocket](https://github.com/fjames86/fsocket) library)
2. we use a simple configuration file to describe the structure of a CAN frame payload
3. we send decoded data to a monitor written with [CLX](https://sharplispers.github.io/clx/)

## How to build
1. Ensure ASDF finds all 3 dependencies (:fsocket :clx and :unix-opts)
2. Go to the directory with sources and execute `make`

## How to test
1. To simulate a CAN bus, use a [virtual](https://elinux.org/Bringing_CAN_interface_up) CAN interface.
2. Optionally, create your own configuration file ([example](./tests/demo-config)) and generate mock-up
CAN data with [this](./tests/gen-data.lisp) script.
2. Build and start the application:
```
make; ./can-logger -p ./demo-config -c vcan0 -s 100 -w 60
```
3. Generate CAN traffic with [cangen-script](./tests/cangen-script.sh)
```
sh ./tests/cangen-script.sh
```
4. Ensure you see on the monitor the data specified in the configuration file.

## How to use
First, create a simple configuration file that describes how CAN data must be interpreted. For example, with this entry
```
can_id = {385}
signal = {analog}
byte_swap = {true}
data_type_mask = {i16, i16, i16, i16}
bit_factor_mask = {610.4e-9, 610.4e-9, 610.4e-9, 610.4e-9}
physical_factor_mask = {101.1, 0, 12.5, 300}
physical_offset_mask = {-50.0, 0, -100.0, -750.0}
label = {Pressure_inlet, None, Mass_flow_rate, Temperature}
```
you say that if the CAN-logger receives a frame like `385 # DE 0D A0 04 E7 00 EE 01`, it should be decoded as follows:
* signal type is analog, i.e. values in the payload are integers. Another option herer would be a _digital_ signal type, where
one encodes just 0s and 1s.
* byte_swap is true means that the little endian format is used
* data type mask is 4 x i16, that means we expect 4 16-bit signed integers decoded in the payload. One can choose between
(_i_)signed and (_u_)unsigned 8/16/32-bit integers.
* bit factors (b), physical factors (f) and physical offset (o) tell you how to convert the raw interger
value to a physical unit (p): p = b*f + o
* labels are tags for these units

Then provide the path to your config file as well as other arguments via CL
```
./can-logger -p "path-to-config-file" -c "name-of-can-interface" -s "sampling-time-in-milliseconds" -w "time-window-width-in-seconds"
```
For example:
```
./can-logger -p ./config -c vcan0 -s 100 -w 60
```
## Tested
Red Hat 4.8.5-44 - SBCL 2.1.9

## Requirements
1. Linux kernel > 2.6.25
2. Libraries: 
    - [fsocket](https://github.com/fjames86/fsocket)
	- [CLX](https://sharplispers.github.io/clx/)
	- [unix-sockets](https://github.com/libre-man/unix-opts)

## Todos
- [ ] Fix autorescaling
- [ ] Decode multiplexed payload
- [ ] Don't run the app if CAN interface doesn't exist

## Acknowledgement
Kudos to Oliver Hartkopp for his brilliant work on the SocketCAN API!

