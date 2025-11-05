WebDAV Explorer
===============

This is a tiny, client-only, web-app for exploring the contents of a WebDAV
repository. This directory is designed to be exposed by a server also serving
WebDAV resources, since that negates any worry about CORS or similar.

It can be mounted as a calp webdav file resource, but care should be taken that
the files aren't overwritten then.

Plain JavaScript is used in favour of TypeScript, to skip a build step.
