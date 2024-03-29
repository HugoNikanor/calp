CALP
====

Calp is primarily a program for loading calendar files (.ics's) from
drendering them in different formats. The goal is however to also
support fancy filtering, an edit server, and more. The currently
working frontends is the HTML-frontend, which have the two main modes
of a month-by-month in "week" view, or a table of a single month, and
the terminal frontend. The terminatend is mostly for debugging
purposes, but it's quite usable still.

Configuration is set in `~/.config/calp/config.scm`. Set at least
calendar-files with something like:

    (set-config! 'calendar-files (glob "~/calendars/*")).
Both single calendar files, and vdir's are supported, see vdirsyncer
and ikhal. Then run

    ./main --help
to see how to start the different modes.

Contributing
------------
Easiest is to open issues at https://github.com/HugoNikanor/calp. But
patches and the like can also be mailed to <hugo@lysator.liu.se>

Requirements & Dependencies
---------------------------
For basic functionallity guile-2.2 or greater should be enough (tested
to work with guile-3.0). You do however need to supply your own
calendar files. I recommend vdirsyncer for fetching local copies from
all over the internet.

The [zoneinfo data][TZ] is in [the public domain][TZLIC].

Standards and specifications
----------------------------
- RFC 5545 (iCalendar)
- RFC 6321 (xCal)
- RFC 7265 (jCal)
- [Vdir Storage Format][VDIR]

Building & Running
------------------
Everything can be directly loaded due to Guile's auto-compilation.
However, two entry points are provided.

- `main`, which sets up its own environment, and explicitly builds all
  libraries before starting, and
- `production-main`, which assumes that the environment already is
  fine, and is the version which should be installed.

The code can also be explicitly manually built, see the makefile.

The environment/make variable `GUILE` can be set to another guile
binary, such as `guile3`.  Guild by defaults also uses this, but if a
separate guild version is explicitly required then the env/make var
`GUILD` can be set (but this shouldn't be needed).

Techical Details
----------------
- Internally all weeks start on sunday, which is repsenented as `0`.

### The configuration system
For all user provided variables a purpose built configuration system
is used. Thee module `(util config)` exposes the bindings
`define-config` along with `set-config` and `get-config`. The idea
behind this, instead of direct variables, is to make it clearer what
is part of the configurable environment, it allows a set! before the
point of definition, and it makes values constraints easier to manage.

[TZ]: https://github.com/eggert/tz
[TZLIC]: https://github.com/eggert/tz/blob/master/LICENSE
[VDIR]: http://vdirsyncer.pimutils.org/en/latest/vdir.html
