/**
 * `<date-time-input />`
 *
 * @category Web Components
 * @mergeTarget components
 * @module
 */

export { DateTimeInput }

import { makeElement } from '../lib'
import { parse_date } from '../datetime'

/**
 * The HTML component `<date-time-input />`.
 * An element for input for date-times. Similar to
 * @example
 * ```html
 * <input type="date"/>
 * <input type="time"/>
 * ```
 *
 * But as a single unit.
 *
 * ### Attributes
 * - dateonly
 *
 */
class DateTimeInput extends /* HTMLInputElement */ HTMLElement {

    /** Our time input element */
    readonly time: HTMLInputElement;
    /** Our date input element */
    readonly date: HTMLInputElement;

    constructor() {
        super();

        this.date = makeElement('input', {
            type: 'date'
        }) as HTMLInputElement

        this.time = makeElement('input', {
            type: 'time',
            disabled: this.dateonly
        }) as HTMLInputElement
    }

    /**
       We set our children first when mounted.

       This can be in the constructor for chromium, but NOT firefox...

       - Vivaldi 4.3.2439.63 stable
       - Mozilla Firefox 94.0.1
    */
    connectedCallback() {
        this.replaceChildren(this.date, this.time)
    }

    /**
       Attributes which we want notifications when they are change.

       Part of the Web Component API

       - `dateonly`
    */
    static get observedAttributes() {
        return ['dateonly']
    }

    /** Part of the Web Component API */
    attributeChangedCallback(name: string, _: string | null, to: string | null): void {
        switch (name) {
            case 'dateonly':
                if (to == null) {
                    this.time.disabled = false
                } else {
                    if (to == '' || to == name) {
                        this.time.disabled = true;
                    } else {
                        throw new TypeError(`Invalid value for attribute dateonly: ${to}`)
                    }
                }
                break;
        }
    }

    /**
       Setting this to true disabled the time part of the input, and makes
       any output only have date components (alternativly, the time component
       set to zero).
    */
    get dateonly(): boolean {
        return this.hasAttribute('dateonly');
    }

    /** See getter */
    set dateonly(b: boolean) {
        if (b) {
            this.setAttribute('dateonly', "");
        } else {
            this.removeAttribute('dateonly');
        }
    }

    /** See getter */
    set value(date: Date) {
        let [d, t] = date.format("~L~Y-~m-~dT~H:~M").split('T');
        this.date.value = d;
        this.time.value = t;

        this.dateonly = date.dateonly;
    }

    /** Returns current value as a Date object. */
    get value(): Date {
        let dt;
        let date = this.date.value;
        if (this.dateonly) {
            dt = parse_date(date);
            dt.dateonly = true;
        } else {
            let time = this.time.value;
            dt = parse_date(date + 'T' + time)
            dt.dateonly = false;
        }
        return dt;
    }

    /** Returns current value as an ISO-8601 formatted string. */
    get stringValue(): string {
        if (this.dateonly) {
            return this.value.format("~Y-~m-~d")
        } else {
            return this.value.format("~Y-~m-~dT~H:~M:~S")
        }
    }

    /**
       Set the selected date.

       @param new_value
       If given a date, set the input to that date.
       If given a string, parse it as an ISO-8601 formatted datetime.
    */
    set stringValue(new_value: Date | string) {
        let date, time, dateonly = false;
        if (new_value instanceof Date) {
            date = new_value.format("~L~Y-~m-~d");
            time = new_value.format("~L~H:~M:~S");
            dateonly = new_value.dateonly;
        } else {
            [date, time] = new_value.split('T')
        }
        this.dateonly = dateonly;
        this.date.value = date;
        this.time.value = time;
    }

    /**
       Adds an event listener to both the date and time input.
    */
    addEventListener(type: string, proc: ((e: Event) => void)) {
        if (type != 'input') throw "Only input supported";

        this.date.addEventListener(type, proc);
        this.time.addEventListener(type, proc);
    }
}
