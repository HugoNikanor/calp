Test suite for Calp
===================

### unit
Unit tests. These are ran by [our testrunner](../testrunner.scm).

### formats
Test of all supported serialization and de-serialization formats.

From a given "master" record, attempt to serialize it into
iCalendar/xCalendar/..., and check against a pre-checked correct serialization.
Also attempt to parse that pre checked serialization, and that it matches the
master record.

### RFC4791
Tests extracted from RFC4791 (Calendaring Extension to WebDAV (CalDAV))

Each directory contains two files:

- `request`, which is an HTTP request which will be sent to the server
- `response` which is the expected HTTP response from the server

TODO write a test runner for these

### Litmus
[Litmus](LITMUS) is a ready-made WebDAV test suite. It was last updated
december 2011, but WebDAV isn't a moving target.

`litmus.scm` starts an instance of our WebDAV server, and runs the litmus suite
against it.


### validate-html
Validate HTML code generated from the backend.

Requires <https://github.com/svenkreiss/html5validator>.

### web

[LITMUS]: http://webdav.org/neon/litmus/
