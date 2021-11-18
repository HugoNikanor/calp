export { DateTimeInput }

import { makeElement, parseDate } from '../lib'


/* '<date-time-input />' */
class DateTimeInput extends /* HTMLInputElement */ HTMLElement {

    connectedCallback() {
        /* This can be in the constructor for chromium, but NOT firefox...
           Vivaldi 4.3.2439.63 stable
           Mozilla Firefox 94.0.1
        */
        /*
 https://developer.mozilla.org/en-US/docs/Web/HTML/Attributes#boolean_attributes
 https://developer.mozilla.org/en-US/docs/Web/API/Element/getAttribute
        */
        this.replaceChildren(
            makeElement('input', { type: 'date' }),
            makeElement('input', {
                type: 'time',
                disabled: this.hasAttribute('dateonly')
            })
        )
    }

    static get observedAttributes() {
        return ['dateonly']
    }

    attributeChangedCallback(name: string, _: string | null, to: string | null): void {
        switch (name) {
            case 'dateonly':
                let time = this.querySelector('input[type="time"]') as HTMLInputElement | null
                /* should only be possible on creation whith dateonly="" sat. */
                if (!time) return;
                if (to == null) {
                    time.disabled = false
                } else {
                    if (to == '' || to == name) {
                        time.disabled = true;
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

    get valueAsDate(): Date {
        let dt;
        let date = (this.querySelector("input[type='date']") as HTMLInputElement).value;
        if (this.hasAttribute('dateonly')) {
            dt = parseDate(date);
            dt.type = 'date';
        } else {
            let time = (this.querySelector("input[type='time']") as HTMLInputElement).value;
            dt = parseDate(date + 'T' + time)
            dt.type = 'date-time';
        }
        return dt;
    }

    get value(): string {
        if (this.dateonly) {
            return this.valueAsDate.format("~Y-~m-~d")
        } else {
            return this.valueAsDate.format("~Y-~m-~dT~H:~M:~S")
        }
    }

    set value(new_value: Date | string) {
        // console.log('Setting date');
        let date, time, dateonly = false;
        if (new_value instanceof Date) {
            date = new_value.format("~L~Y-~m-~d");
            time = new_value.format("~L~H:~M:~S");
            dateonly = new_value.dateonly;
        } else {
            [date, time] = new_value.split('T')
        }
        this.dateonly = dateonly;
        (this.querySelector("input[type='date']") as HTMLInputElement).value = date;
        (this.querySelector("input[type='time']") as HTMLInputElement).value = time;
    }

    addEventListener(type: string, proc: ((e: Event) => void)) {
        if (type != 'input') throw "Only input supported";

        (this.querySelector("input[type='date']") as HTMLInputElement)
            .addEventListener(type, proc);
        (this.querySelector("input[type='time']") as HTMLInputElement)
            .addEventListener(type, proc);
    }
}
