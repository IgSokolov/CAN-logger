## Introduction
`CAN-logger` is a CAN bus monitor, which is intended to be used for debugging of CAN buses in a laboratory environment.
<p align="center">
	<img src=gui-demo.png width=100% height=100%>
</p>

## How it works
1. we access CAN bus via [SocketCAN](https://www.kernel.org/doc/html/latest/networking/can.html) (using FFI from [fsocket](https://github.com/fjames86/fsocket) library)
2. we use a simple configuration file to describe the structure of a CAN frame payload
3. we send decoded data to a monitor written with [CLX](https://sharplispers.github.io/clx/)

## How to build
Go to the directory with sources and execute `make`

## How to test
TBD

## How to use
First, create a simple config file that describes how CAN data must be interpreted. For example, with this entry
```
can_id = {385}
signal = {analog}
byte_swap = {true}
data_type_mask = {i16, i16, i16, i16}
bit_factor_mask = {610.4e-9, 610.4e-9, 610.4e-9, 610.4e-9}
physical_factor_mask = {12500.0, 1, 25000.0, 187500.0}
physical_offset_mask = {-50.0, 0, -100.0, -750.0}
label = {MFM_H2_Ist_nl_min,None,Hall_Sensor_Strom_Ist_A, MFC_Luft_Ist_nl_min}
```
you say that if the CAN-logger receives a frame like 385#aa.bb.cc.dd.ee.ff.00.11, it should be decoded as follows:
* signal type is analog, i.e. values in the payload are floats. Another option herer would be a _digital_ signal type, where
one encodes just 0s and 1s.
* byte_swap is true means that the little endian format is used
* data type mask is 4 x i16, that means we expect 4 16-bit signed integers decoded in the payload. One can choose between
(_i_)signed and (_u_)unsigned 8/16/32-bit integers.
* bit factors (b), physical factors (f) and physical offset (o) tell you how to convert the raw interger
value to a physical unit (p): p = b*f + o
* labels are tags for these units
```
./can-logger -p "path-to-config-file" -c "name-of-can-interface"
```
## Tested
SBCL 2.1.9

## Todos


## Contribution:
TBD
