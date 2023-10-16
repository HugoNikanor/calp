Data format tests
=================

These aren't really unit tests, but run fine through the same
framework.

The file [`run.scm`](run.scm) contains a reference calendar (or
"master record") component which should contain all weird cases which
may be encountered. It then tries to serialize this component to all
registered serialization formats (iCalendar, xCalendar, ...) and
checks it against a pre vetted reference file.

It then takes the serialized form and parses it back into a Guile data
structure, and checks it against the "master record".
