export { ComponentEdit }

import { ComponentVEvent } from './vevent'
import { DateTimeInput } from './date-time-input'

import { vcal_objects, event_calendar_mapping } from '../globals'
import { VEvent } from '../vevent'
import { create_event } from '../server_connect'

/* <vevent-edit />
   Edit form for a given VEvent. Used as the edit tab of popups.
*/
class ComponentEdit extends ComponentVEvent {

    firstTime: boolean

    constructor() {
        super();

        this.firstTime = true;
    }

    connectedCallback() {

        /* Edit tab is rendered here. It's left blank server-side, since
           it only makes sense to have something here if we have javascript */

        let data = vcal_objects.get(this.uid)

        if (!data) {
            throw `Data missing for uid ${this.dataset.uid}.`
        }


        // return;

        /* Handle calendar dropdown */
        for (let el of this.getElementsByClassName('calendar-selection')) {
            for (let opt of el.getElementsByTagName('option')) {
                opt.selected = false;
                if (opt.value == event_calendar_mapping.get(this.uid)) {
                    data.setCalendar(opt.value);
                    opt.selected = true;
                    /* No break since we want to set the remainders 'selected' to false */
                }
            }

            el.addEventListener('change', (e) => {
                let v = (e.target as HTMLSelectElement).selectedOptions[0].value
                // e.selectedOptions[0].innerText

                let obj = vcal_objects.get(this.uid)!
                obj.setCalendar(v);
            });
        }

        this.redraw(data);

        for (let el of this.getElementsByClassName("interactive")) {
            // console.log(el);
            el.addEventListener('input', () => {
                let obj = vcal_objects.get(this.uid)
                if (obj === undefined) {
                    throw 'No object with uid ' + this.uid
                }
                if (!(el instanceof HTMLInputElement
                    || el instanceof DateTimeInput)) {
                    console.log(el, 'not an HTMLInputElement');
                    return;
                }
                obj.setProperty(
                    el.dataset.property!,
                    el.value)
            });
        }

        let submit = this.querySelector('form') as HTMLFormElement
        submit.addEventListener('submit', (e) => {
            console.log(submit, e);
            create_event(vcal_objects.get(this.uid)!);

            e.preventDefault();
            return false;
        });
    }

    redraw(data: VEvent) {
        // update ourselves from template

        if (!this.template) {
            throw "Something";
        }

        let body;
        if (this.firstTime) {
            body = (this.template.content.cloneNode(true) as DocumentFragment).firstElementChild!;
        } else {
            body = this;
        }

        for (let el of body.getElementsByClassName("interactive")) {
            if (!(el instanceof HTMLElement)) continue;
            let p = el.dataset.property!;
            let d: any;
            if ((d = data.getProperty(p))) {
                /*
                  https://stackoverflow.com/questions/57157830/how-can-i-specify-the-sequence-of-running-nested-web-components-constructors
                */
                window.setTimeout(() => {
                    /* NOTE Some specific types might require special formatting
                    here. But due to my custom components implementing custom
                    `.value' procedures, we might not need any special cases
                    here */
                    /* Technically we just want to cast to HTMLElement with
                    value field here, but multiple types implement it
                    sepparately, and no common interface exist */
                    (el as HTMLInputElement).value = d;
                });
            }
        }

        for (let el of body.getElementsByTagName('calendar-selection')) {
            for (let opt of el.getElementsByTagName('option')) {
                opt.selected = false;
                if (opt.value == data._calendar) {
                    opt.selected = true;
                }
            }
        }

        if (this.firstTime) {
            this.replaceChildren(body);
            this.firstTime = false;
        }
    }

}
