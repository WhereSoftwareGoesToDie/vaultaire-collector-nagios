vaultaire-collector-nagios
==========================

vaultaire-collector-nagios reads Nagios perfdata from stdin and writes it
to [Vaultaire](https://github.com/anchor/vaultaire).

dependencies
============

 - [Marquise](https://github.com/anchor/marquise) 
 - [nagios-perfdata](https://github.com/anchor/nagios-perfdata)

operation
=========

There are a few ways to set this up; at its core the only thing that
needs to be done is to obtain Nagios perfdata files somehow and pipe
them into this collector. One way to do this if you currently use
pnp4nagios/npcd for perfdata processing is to add a line to the
process_perfdata.pl script invoking this program - an example of such a
modification is included in this repository. Of course, this is quite
awful; there will be better ways of doing this in the future (although
if you are using
[mod_gearman](https://labs.consol.de/nagios/mod-gearman/) then [there is
a better
alternative](https://github.com/anchor/vaultaire-collector-nagios-gearman)).
