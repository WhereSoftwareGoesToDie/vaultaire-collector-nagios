vaultaire-collector-nagios
==========================

vaultaire-collector-nagios reads Nagios perfdata from stdin and writes it
to [Vaultaire](https://github.com/anchor/vaultaire).

Dependencies
============

 - [Marquise](https://github.com/anchor/marquise) 
 - [nagios-perfdata](https://github.com/anchor/nagios-perfdata)
 - [gearman-haskell](https://github.com/anchor/gearman-haskell)

Operation
=========

There are a few ways to set this up; at its core the only thing that
needs to be done is to obtain Nagios perfdata files somehow and pipe
them into this collector. One way to do this if you currently use
pnp4nagios/npcd for perfdata processing is to add a line to the
`process_perfdata.pl` script invoking this program - an example of such a
modification is included in this repository. Of course, this is quite
awful; there will be better ways of doing this in the future.

However, if you are using [`mod_gearman`](https://labs.consol.de/nagios/mod-gearman/)
then you can use this collector in gearman mode, which will set up
gearman worker threads to do the writing.

Metadata
========

In addition to metric-identifying fields (`host`, `metric`, and
`service`), the collector will write the following presentation-layer
metadata for each datapoint:

 - `_unit` - whether or not the metric has been normalized (i.e., scaled
   to base units per SI prefix).
 - `_uom` - Nagios UOM. May be different from that reported by the check
   result, if scaling is applied.
 - `_counter` - whether the metric is marked as a counter in the Nagios
   check output.
 - `_float` - whether the datapoint word is floating-point (as opposed
   to integral).
