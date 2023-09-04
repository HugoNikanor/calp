export { DateTimeInput }

import { makeElement, parseDate } from '../lib'


/* '<date-time-input />' */
class DateTimeInput extends /* HTMLInputElement */ HTMLElement {

    readonly time: HTMLInputElement;
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

    connectedCallback() {
        /* This can be in the constructor for chromium, but NOT firefox...
           Vivaldi 4.3.2439.63 stable
           Mozilla Firefox 94.0.1
        */
        /*
 https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes#boolean_attributes
 https://developer.mozilla.org/en-US/docs/Web/API/Element/getAttribute
        */
        this.replaceChildren(this.date, this.time)
    }

    static get observedAttributes() {
        return ['dateonly']
    }

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

    get dateonly(): boolean {
        return this.hasAttribute('dateonly');
    }

    set dateonly(b: boolean) {
        if (b) {
            this.setAttribute('dateonly', "");
        } else {
            this.removeAttribute('dateonly');
        }
    }

    set value(date: Date) {
        let [d, t] = date.format("~L~Y-~m-~dT~H:~M").split('T');
        this.date.value = d;
        this.time.value = t;

        this.dateonly = date.dateonly;
    }

    get value(): Date {
        let dt;
        let date = this.date.value;
        if (this.dateonly) {
            dt = parseDate(date);
            dt.dateonly = true;
        } else {
            let time = this.time.value;
            dt = parseDate(date + 'T' + time)
            dt.dateonly = false;
        }
        return dt;
    }

    get stringValue(): string {
        if (this.dateonly) {
            return this.value.format("~Y-~m-~d")
        } else {
            return this.value.format("~Y-~m-~dT~H:~M:~S")
        }
    }

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

    addEventListener(type: string, proc: ((e: Event) => void)) {
        if (type != 'input') throw "Only input supported";

        this.date.addEventListener(type, proc);
        this.time.addEventListener(type, proc);
    }
}
