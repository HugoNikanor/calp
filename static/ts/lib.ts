/**
  General procedures which in theory could be used anywhere.

  Besides exporting the mentioned functions, this module also
  extends some base classes.

  @module
 */
export {
    makeElement, date_to_percent,
    gensym, to_local, to_boolean,
    asList, round_time,
}

import { format_date, to_local } from './datetime'

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
