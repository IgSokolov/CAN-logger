#!/usr/bin/sh

# $1 = can-id
# $2 = config path

body="can_id = {$1}\nsignal = {analog}\nbyte_swap = {true}\ndata_type_mask = {i16, i16, i16, i16}\nbit_factor_mask = {1.1e-9, 1.1e-9, 1.1e-9, 1.1e-9}\nphysical_factor_mask = {1.0, 1.0, 1.0, 1.0}\nphysical_offset_mask = {0.0, 0.0, 0.0, 0.0}\nlabel = {label-1, label-2, label-3, label-4}"

echo -ne $body > tmp_entry
gedit tmp_entry
