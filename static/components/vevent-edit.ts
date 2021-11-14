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

    constructor() {
        super();

        let frag = this.template.content.cloneNode(true) as DocumentFragment
        let body = frag.firstElementChild!
        this.replaceChildren(body);
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
            }
            if (data.calendar) {
                (el as HTMLSelectElement).value = data.calendar;
            }

            el.addEventListener('change', (e) => {
                let v = (e.target as HTMLSelectElement).selectedOptions[0].value
                let obj = vcal_objects.get(this.uid)!
                obj.calendar = v;
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
        /* We only update our fields, instead of reinstansiating
           ourselves from the template, in hope that it's faster */

        for (let el of this.getElementsByClassName("interactive")) {
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

        if (data.calendar) {
            for (let el of this.getElementsByClassName('calendar-selection')) {
                (el as HTMLSelectElement).value = data.calendar;
            }
        }
    }
}
