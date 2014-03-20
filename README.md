vaultaire-collector-nagios
==========================

vaultaire-collector-nagios reads Nagios perfdata from stdin and writes it
to vaultaire (and potentially other backends, though only one is
currently enabled). 


usage
=====

Copy bletchleyrc.sample to ~/.bletchleyrc (or wherever you like, just 
modify the invocation of vaultaire-collector-nagios to pass -cfg <path>).

Drop the modified process_perfdata.pl in place of the one used to write
metrics to RRD. By default, the supplied process_perfdata.pl will look
for the bletchley config file at /usr/local/etc/bletchleyrc.

