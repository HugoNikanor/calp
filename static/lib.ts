export {
    makeElement, date_to_percent, uid,
    parseDate, gensym, to_local, boolean,
    xcal, asList, round_time
}

/*
  General procedures which in theory could be used anywhere.
 */

/*
 * https://www.typescriptlang.org/docs/handbook/declaration-merging.html
 */
declare global {
    interface Object {
        format: (fmt: string) => string
    }

    interface HTMLElement {
        _addEventListener: (name: string, proc: (e: Event) => void) => void
        listeners: Record<string, ((e: Event) => void)[]>
    }

    interface Date {
        format: (fmt: string) => string
        dateonly: boolean
        utc: boolean
        type: 'date' | 'date-time'
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

type uid = string

HTMLElement.prototype._addEventListener = HTMLElement.prototype.addEventListener;
HTMLElement.prototype.addEventListener = function(name: string, proc: (e: Event) => void) {
    if (!this.listeners) this.listeners = {};
    if (!this.listeners[name]) this.listeners[name] = [];
    this.listeners[name].push(proc);
    return this._addEventListener(name, proc);
};



/* list of lists -> list of tuples */
/* TODO figure out how to type this correctly */
function zip(...args: any[]) {
    // console.log(args);
    if (args === []) return [];
    return [...Array(Math.min(...args.map(x => x.length))).keys()]
        .map((_, i) => args.map(lst => lst[i]));
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

function copyDate(date: Date): Date {
    let d = new Date(date);
    d.utc = date.utc;
    d.dateonly = date.dateonly;
    return d;
}

function to_local(date: Date): Date {
    if (!date.utc) return date;

    return new Date(date.getTime() - date.getTimezoneOffset() * 60 * 1000);
}

/* -------------------------------------------------- */

function makeElement(name: string, attr = {}): HTMLElement {
    let element: HTMLElement = document.createElement(name);
    for (let [key, value] of Object.entries(attr)) {
        (element as any)[key] = value;
    }
    return element;
}

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
function date_to_percent(date: Date): number /* in 0, 100 */ {
    return (date.getHours() + (date.getMinutes() / 60)) * 100 / 24;
}

/* if only there was such a thing as a let to wrap around my lambda... */
/* js infix to not collide with stuff generated backend */
const gensym = (counter => (prefix = "gensym") => prefix + "js" + ++counter)(0)

function setVar(str: string, val: any) {
    document.documentElement.style.setProperty("--" + str, val);
}


function asList<T>(thing: Array<T> | T): Array<T> {
    if (thing instanceof Array) {
        return thing;
    } else {
        return [thing];
    }
}


function boolean(value: any): boolean {
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

const xcal = "urn:ietf:params:xml:ns:icalendar-2.0";
