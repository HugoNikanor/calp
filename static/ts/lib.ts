/**
  General procedures which in theory could be used anywhere.

  Besides exporting the mentioned functions, this module also
  extends some base classes.

  @module
 */
export {
    makeElement, date_to_percent,
    parseDate, gensym, to_local, to_boolean,
    asList, round_time,
    format_date,
}

/*
 * https://www.typescriptlang.org/docs/handbook/declaration-merging.html
 */
declare global {
    interface Object {
        /**
           Introduce a format method on ALL objects

           The default implementation simply stringifies the object, but this
           allows any component to overwrite it, allowing for generic custom
           formatting of data.

           This also means that the format string is ignored for the default
           implementation.

           See `Data.prototype.format`.
        */
        format: (fmt: string) => string
    }

    /** HTMLElement extensions */
    interface HTMLElement {
        /**
           "Private" property, storing the "true" add event listener. The
           exposed addEventListener is later overwritten to also store a list of
           which event listeners are added.
        */
        _addEventListener: (name: string, proc: (e: Event) => void) => void

        /**
           Contains all listeners added through `addEventListener`.

           The keys are the same as to `addEventListener` ('load', 'mouseover',
           ...)

           Values are simply a list of all added listeners, probably in addition
           order.
        */
        listeners: Map<string, ((e: Event) => void)[]>;

        /**
           Returns listeners.

           TODO why does this exist?
        */
        getListeners: () => Map<string, ((e: Event) => void)[]>
    }

    interface Date {
        /**
           A proper date formatter for javascript.

           See {@ref format_date} for details
        */
        format: (fmt: string) => string

        /** Indicates if the date object is in UTC or local time. */
        utc: boolean

        /**
           Indicates that the object only contains a date component.

           This means that any time is ignored in most operations.
        */
        dateonly: boolean
        // type: 'date' | 'date-time'
    }

    interface DOMTokenList {
        /**
           Searches a DOMTokenList for anything matching.

           DOMTokenLists are returned by `.classList` and similar.

           @return Returns the matching index, and the matched value,
              or `undefined` if nothing was found.
           */
        find: (regex: string) => [number, string] | undefined
    }

    interface HTMLCollection {
        /** Adds an iterator to HTMLCollections */
        forEach: (proc: ((el: Element) => void)) => void
    }

    interface HTMLCollectionOf<T> {
        /** Adds an iterator to HTMLCollections */
        forEach: (proc: ((el: T) => void)) => void
    }
}

/** See interface above */
HTMLElement.prototype._addEventListener = HTMLElement.prototype.addEventListener;
/** See interface above */
HTMLElement.prototype.addEventListener = function(name: string, proc: (e: Event) => void) {
    if (!this.listeners) this.listeners = new Map
    if (!this.listeners.get(name)) this.listeners.set(name, []);
    /* Force since we ensure a value just above */
    this.listeners.get(name)!.push(proc);
    return this._addEventListener(name, proc);
};
/** See interface above */
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

/**
   Round clock time to closest interval.

   @param time
   The desired clock-time, in decimal time. So 12:30 would be given as 12.30.

   @param fraction
   The time interval to round to. To round to nearest half hour, give 0.5.

   @example
   ```js
   > round_time(10.1, 15/60)
   10
   ```
   */
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


/**
   Smartly converts a value into a boolean.

   Booleans are returned as if,

   Strings are parsed, mapping `'true'` onto `true`, `'false'` onto `false`,
   empty strings onto `false`, and anything else onto `true`.

   Anything else is left onto JavaScript to coerce a boolean.
   */
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

/**
   Format a date into a string.

   @param date
   The datetime to format

   @param format
   How the date should be converted into a string.

   The format is similar to `strftime`, but with tilde (`~`) characters
   instead of percent signs, to match how Scheme does it. Valid format
   specifiers are:

   | Fmt  |               Output             | Width¹ |
   |------|----------------------------------|--------|
   | `~~` | Literal '~'                      |        |
   | `~Y` | Year                             | 4      |
   | `~m` | Month number                     | 2      |
   | `~d` | Day of month                     | 2      |
   | `~H` | Hour                             | 2      |
   | `~M` | Minute                           | 2      |
   | `~S` | Second                           | 2      |
   | `~Z` | 'Z' if date is UTC, otherwise '' |        |
   | `~L` | Converts date to local time²     |        |

   - ¹ These fields will be left padded with zeroes to that width
   - ² This forces the output to be in local time, possibly converting
     timezone if needed. It then outputs nothing.
     See {@link to_local `to_local`} for details.

*/
function format_date(date: Date, format: string): string {
    let fmtmode = false;
    let outstr = "";
    for (var i = 0; i < format.length; i++) {
        if (fmtmode) {
            switch (format[i]) {
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
        } else if (format[i] == '~') {
            fmtmode = true;
        } else {
            outstr += format[i];
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
