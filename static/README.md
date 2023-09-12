The frontend code has its entry-point in `script.ts`.

Much of this code assumes prior knowledge of the iCalendar standard (RFC
5545). Besides that, the term "VComponent" is used to refer to any Calendar
Component as specified in that RFC under 3.6.

## Data Flow

TODO document how data gets from the server to us, and from us to the server.

A large part of this is how much we convert between serialization formats.

## web components

All elements are initialized in components.ts

#### Boolean attributes
Some components have properties/accessors which also appears as attributes on
the actuall component (usually with a two-way maping).

For boolean attributes, the attribute is either present or absent.

### General Components
- `components/date-time-input.ts`
- `components/input-list.ts`

### VEvent Components
- `components/vevent.ts`
- `components/changelog.ts`
- `components/edit-rrule.ts`
- `components/popup-element.ts`
- `components/tab-group-element.ts`
- `components/vevent-block.ts`
- `components/vevent-description.ts`
- `components/vevent-dl.ts`
- `components/vevent-edit.ts`

## About our buildsystem

Currently (almost) everything is written in Typescript, and bundled
through browserify. Ideally we would, for debug builds, export the
single transplied Javascript files, but Chromium Chromium lacks
support for modules on XHTML documents
https://bugs.chromium.org/p/chromium/issues/detail?id=717643.
However, seeing as the issue still gets frequent updates as of 2021 I
believe that this might one day get resolved.
