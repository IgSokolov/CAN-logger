## Introduction
`CAN-logger` is a CAN bus monitor, which is intended to be used for debugging of CAN buses in a laboratory environment.
<p align="center">
	<img src=demo.png width=100% height=100%>
</p>
I devote this project to Oliver Hartkopp - a man behind the ingenious idea of the SocketCAN API.

## How it works
1. we access CAN bus via [SocketCAN](https://www.kernel.org/doc/html/latest/networking/can.html) (using FFI from [fsocket](https://github.com/fjames86/fsocket) library)
2. we use a simple configuration file to describe the structure of a CAN frame payload
3. we send decoded data to a monitor written with [CLX](https://sharplispers.github.io/clx/)
### Implementation notes
1. Each CAN-id becomes a new random color on the plot
2. Autorescale
3. If the number of CAN-ids exceeds the number of lines in the table, a new table window is
created on top of the prevoíous one. Use arrow buttons to switch to other windows on the stack.

## How to build
Go to the directory with sources and execute `make`

## How to test
To make the life easy, use a [virtual](https://elinux.org/Bringing_CAN_interface_up) CAN interface.

## How to use
First, create a simple config file that describes how CAN data must be interpreted. For example, with this entry
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
you say that if the CAN-logger receives a frame like 385#aa.bb.cc.dd.ee.ff.a1.a2, it should be decoded as follows:
* signal type is analog, i.e. values in the payload are integers. Another option herer would be a _digital_ signal type, where
one encodes just 0s and 1s.
* byte_swap is true means that the little endian format is used
* data type mask is 4 x i16, that means we expect 4 16-bit signed integers decoded in the payload. One can choose between
(_i_)signed and (_u_)unsigned 8/16/32-bit integers.
* bit factors (b), physical factors (f) and physical offset (o) tell you how to convert the raw interger
value to a physical unit (p): p = b*f + o
* labels are tags for these units
Note that CAN frames not listed in the config file are discarded by the logger.

Then provide the path to your config file as well the name of a CAN interface via CLI:
```
./can-logger -p "path-to-config-file" -c "name-of-can-interface"
```
Note that this software is still under heavy development and is not production ready.
## Tested
SBCL 2.1.9

## Requirements
Linux kernel > 2.6.25

## Todos
- [x] Make CLI interface with :unix-opts
- [ ] Show CAN-IDs of the frames not listed in config file
- [ ] Decode binary payload (valve postions, on/off flags, etc.)
- [ ] Decode multiplexed payload
- [ ] Make _stop_ button
- [ ] Make _reload config_ button
- [x] Add CAN-ids or labels on the plot
- [ ] Add relevant CAN-bus statistics
- [ ] Dont't pick up dark colors for lines 
