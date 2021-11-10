export { DateTimeInput }

import { to_boolean, parseDate } from '../lib'

/* '<date-time-input />' */
class DateTimeInput extends /* HTMLInputElement */ HTMLElement {
    connectedCallback() {
        /* This can be in the constructor for chromium, but NOT firefox...
           Vivaldi 4.3.2439.63 stable
           Mozilla Firefox 94.0.1
        */
        this.innerHTML = '<input type="date" /><input type="time" />'
        // console.log('constructing datetime input')
    }

    static get observedAttributes() {
        return ['dateonly']
    }

    attributeChangedCallback(name: string, _: any, to: any): void {
        // console.log(this, name, to_boolean(from), to_boolean(to));
        switch (name) {
            case 'dateonly':
                (this.querySelector('input[type="time"]') as HTMLInputElement)
                    .disabled = to_boolean(to)
                break;
        }
    }

    get dateonly(): boolean {
        return to_boolean(this.getAttribute('dateonly'));
    }

    set dateonly(bool: boolean) {
        this.setAttribute('dateonly', "" + bool);
    }

    get valueAsDate(): Date {
        let dt;
        let date = (this.querySelector("input[type='date']") as HTMLInputElement).value;
        if (to_boolean(this.getAttribute('dateonly'))) {
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
        return this.valueAsDate.format("~Y-~m-~dT~H:~M:~S")
    }

    set value(new_value: Date | string) {
        // console.log('Setting date');
        let date, time;
        if (new_value instanceof Date) {
            date = new_value.format("~L~Y-~m-~d");
            time = new_value.format("~L~H:~M:~S");
        } else {
            [date, time] = new_value.split('T')
        }
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
