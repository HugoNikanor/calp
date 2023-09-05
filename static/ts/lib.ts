/**
 * General procedures which in theory could be used anywhere.
 *
 * Besides exporting the mentioned functions, this module also extends some base
 * classes.
 *
 * ```tex
@node Default prototype extensions
@subsubsection Default prototype extensions



@defmethod HTMLElement addEventListener name proc
Replace the default @code{addEventListener} with a version that stores
all listeners in the dictionary @var{listeners}.
@end defmethod

@defivar HTMLElement listeners
Dictionary of all registered listeners to this element.
Keys are taken from @code{addEventListener}.
@end defivar

@defmethod DOMTokenList find regexp
Finds the first element of the DOMTokenList whichs value matches
the supplied regexp. Returns a pair of the index and the value.
@end defmethod

@defmethod Object format args ...
Returns a string representation of the given object.
Allows extending for custom types,
@ref{date-format}
@end defmethod
 * ```
 *
 * ---
 *
 * ```tex
Some extensions to the builtin class ``Date'' is made.

@defivar Date utc
Boolean indicating if the given timestamp is in UTC or local time.
true means UTC.
@end defivar

@defivar Date dateonly
Boolean indicating if the time component of the Date object should be disregarded.
@end defivar

 * ```
 * ```tex
@defmethod Date format str args ...
@anchor{date-format}
Formats a Date object according to the format specification @var{str}.
Keeping with Guile each format specifier starts with a ~.

@table @samp
@item ~~
literal ~
@c Almost all fields are left padded. How do I signify this
@c with a single footnote?
@item ~Y
year, left-padding with zeroes.
@item ~m
month number, left padded with zeroes.
@item ~d
day of month.
@item ~H
hour
@item ~M
minute
@item ~S
second
@item ~Z
'Z' if Date is UTC, otherwise nothing
@item ~L
Converts the date to local time
(@pxref{to_local}) (doesn't modify source object). Outputs nothing
@end table
@end defmethod
```
 *
 * @module
 */
export {
    makeElement, date_to_percent,
    parseDate, gensym, to_local, to_boolean,
    asList, round_time
}

/*
 * https://www.typescriptlang.org/docs/handbook/declaration-merging.html
 */
declare global {
    interface Object {
        format: (fmt: string) => string
    }

    /** HTMLElement extensions */
    interface HTMLElement {
        _addEventListener: (name: string, proc: (e: Event) => void) => void
        listeners: Map<string, ((e: Event) => void)[]>
        getListeners: () => Map<string, ((e: Event) => void)[]>
    }

    interface Date {
        format: (fmt: string) => string
        utc: boolean
        dateonly: boolean
        // type: 'date' | 'date-time'
    }

    interface DOMTokenList {
        find: (regex: string) => [number, string] | undefined
    }

    interface HTMLCollection {
        forEach: (proc: ((el: Element) => void)) => void
    }

    interface HTMLCollectionOf<T> {
        forEach: (proc: ((el: T) => void)) => void
    }
}

HTMLElement.prototype._addEventListener = HTMLElement.prototype.addEventListener;
HTMLElement.prototype.addEventListener = function(name: string, proc: (e: Event) => void) {
    if (!this.listeners) this.listeners = new Map
    if (!this.listeners.get(name)) this.listeners.set(name, []);
    /* Force since we ensure a value just above */
    this.listeners.get(name)!.push(proc);
    return this._addEventListener(name, proc);
};
HTMLElement.prototype.getListeners = function() {
    return this.listeners;
}


/* ----- Date Extensions ---------------------------- */

/*
  Extensions to Javascript's Date to allow representing times
  with different timezones. Currently only UTC and local time
  are supported, but more should be able to be added.

  NOTE that only the raw `get' (and NOT the `getUTC') methods
  should be used on these objects, and that the reported timezone
  is quite often wrong.

  TODO The years between 0 and 100 (inclusive) gives dates in the twentieth
  century, due to how javascript works (...).
 */

/**
 * Takes a string `str`, which should be in ISO-8601 date-format, and
 * returns a javascript Date object.
 * Handles date-times, with and without seconds, trailing `Z' for
 * time-zones, and dates without times.
 * If no time is given the `dateonly` attribute is set to yes.
 */
function parseDate(str: string): Date {
    let year: number;
    let month: number;
    let day: number;
    let hour: number | false = false;
    let minute: number = 0;
    let second: number = 0;
    let utc: boolean = false;

    let end = str.length - 1;
    if (str[end] == 'Z') {
        utc = true;
        str = str.substring(0, end);
    };

    switch (str.length) {
        case '2020-01-01T13:37:00'.length:
            second = +str.substr(17, 2);
        case '2020-01-01T13:37'.length:
            hour = +str.substr(11, 2);
            minute = +str.substr(14, 2);
        case '2020-01-01'.length:
            year = +str.substr(0, 4);
            month = +str.substr(5, 2) - 1;
            day = +str.substr(8, 2);
            break;
        default:
            throw `"${str}" doesn't look like a date/-time string`
    }

    let date;
    if (hour) {
        date = new Date(year, month, day, hour, minute, second);
        date.utc = utc;
        date.dateonly = false;
    } else {
        date = new Date(year, month, day);
        date.dateonly = true;
    }
    return date;
}

/* @anchor{to_local} */
/**
 * Returns a Date object (which may be new) which is guaranteed in local time.
 * This means that the `utc` field is `false`, and that
 * `to_local(current_time())` should show what your wall-clock shows.
 */
function to_local(date: Date): Date {
    if (!date.utc) return date;

    return new Date(date.getTime() - date.getTimezoneOffset() * 60 * 1000);
}

/* -------------------------------------------------- */

/**
 * Creates a new DOM element of type `name`, with all keys in
 * `attr` transfered to it. For example, the equivalent of

 * ```html
 * <input type='number'/>
 * ```

 * would be

 * ```js
 * values.push(makeElement('input', {
 *     type: 'number',
 * }));
 * ```
 *
 * @param name HTML tagname
 * @param attr Attributes which will be set on the created element.
 * @param actualAttr Attributes which will be set on the created element,
 *    but through `el.setAttribute` instead of `el[key] =`...
 */
function makeElement(name: string, attr = {}, actualAttr = {}): HTMLElement {
    let element: HTMLElement = document.createElement(name);
    for (let [key, value] of Object.entries(attr)) {
        (element as any)[key] = value;
    }
    for (let [key, value] of Object.entries(actualAttr)) {
        element.setAttribute(key, '' + value);
    }
    return element;
}

/** TODO document me */
function round_time(time: number, fraction: number): number {
    let scale = 1 / fraction;
    return Math.round(time * scale) / scale;
}

/* only used by the bar.
   Events use the start and end time of their container, but since the bar
   is moving between containers that is clumsy.
   Just doing (new Date()/(86400*1000)) would be nice, but there's no good
   way to get the time in the current day.
 */
/**
 * Retuns how far along the date specified by `date` is, between 0
 * and 100, where 00:00 maps to 0, and 23:59 to ~100.
  */
function date_to_percent(date: Date): number /* in 0, 100 */ {
    return (date.getHours() + (date.getMinutes() / 60)) * 100 / 24;
}

/* if only there was such a thing as a let to wrap around my lambda... */
/* js infix to not collide with stuff generated backend */
/**
 * Generates a new string which is (hopefully) globally unique.
 * Compare with `gensym` from Lisp.
 */
const gensym = (counter => (prefix = "gensym") => prefix + "js" + ++counter)(0)

/**
 * Ensures that `thing` is a list. Returning it outright if it
 * already is one, otherwise wrapping it in a list.
*/
function asList<T>(thing: Array<T> | T): Array<T> {
    if (thing instanceof Array) {
        return thing;
    } else {
        return [thing];
    }
}


function to_boolean(value: any): boolean {
    switch (typeof value) {
        case 'string':
            switch (value) {
                case 'true': return true;
                case 'false': return false;
                case '': return false;
                default: return true;
            }
        case 'boolean':
            return value;
        default:
            return !!value;
    }
}



/* internal */
function datepad(thing: number | string, width = 2): string {
    return (thing + "").padStart(width, "0");
}

/** Equivalent to `date.format(str)` */
function format_date(date: Date, str: string): string {
    let fmtmode = false;
    let outstr = "";
    for (var i = 0; i < str.length; i++) {
        if (fmtmode) {
            switch (str[i]) {
                /* Moves the date into local time. */
                case 'L': date = to_local(date); break;
                case 'Y': outstr += datepad(date.getFullYear(), 4); break;
                case 'm': outstr += datepad(date.getMonth() + 1); break;
                case 'd': outstr += datepad(date.getDate()); break;
                case 'H': outstr += datepad(date.getHours()); break;
                case 'M': outstr += datepad(date.getMinutes()); break;
                case 'S': outstr += datepad(date.getSeconds()); break;
                case 'Z': if (date.utc) outstr += 'Z'; break;
            }
            fmtmode = false;
        } else if (str[i] == '~') {
            fmtmode = true;
        } else {
            outstr += str[i];
        }
    }
    return outstr;
}

Object.prototype.format = function(/* any number of arguments */) { return "" + this; }
Date.prototype.format = function(str) { return format_date(this, str); }

/*
 * Finds the first element of the DOMTokenList whichs value matches
 * the supplied regexp. Returns a pair of the index and the value.
 */
DOMTokenList.prototype.find = function(regexp) {
    let entries = this.entries();
    let entry;
    while (!(entry = entries.next()).done) {
        if (entry.value[1].match(regexp)) {
            return entry.value;
        }
    }
}

/* HTMLCollection is the result of a querySelectorAll */
HTMLCollection.prototype.forEach = function(proc) {
    for (let el of this) {
        proc(el);
    }
}
