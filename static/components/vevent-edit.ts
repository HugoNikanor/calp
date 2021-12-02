export { ComponentEdit }

import { ComponentVEvent } from './vevent'
import { InputList } from './input-list'
import { DateTimeInput } from './date-time-input'

import { vcal_objects } from '../globals'
import { VEvent, RecurrenceRule } from '../vevent'
import { create_event } from '../server_connect'

/* <vevent-edit />
   Edit form for a given VEvent. Used as the edit tab of popups.
*/
class ComponentEdit extends ComponentVEvent {

    constructor(uid?: string) {
        super(uid);

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

        // for (let el of this.getElementsByClassName("interactive")) {
        for (let el of this.querySelectorAll("[data-property]")) {
            // console.log(el);
            el.addEventListener('input', (e) => {
                let obj = vcal_objects.get(this.uid)
                // console.log(el, e);
                if (obj === undefined) {
                    throw 'No object with uid ' + this.uid
                }
                if (!(el instanceof HTMLInputElement
                    || el instanceof DateTimeInput
                    || el instanceof HTMLTextAreaElement
                    || el instanceof InputList
                )) {
                    console.log(el, 'not an HTMLInputElement');
                    return;
                }
                // console.log(`obj[${el.dataset.property!}] = `, el.value);
                obj.setProperty(
                    el.dataset.property!,
                    el.value)
            });
        }

        let wholeday_ = this.querySelector('[name="wholeday"]')
        if (wholeday_) {
            let wholeday = wholeday_ as HTMLInputElement

            if (data.getProperty('dtstart')?.dateonly) {
                wholeday.checked = true;
            }

            wholeday.addEventListener('click', () => {
                let chk = wholeday.checked
                let start = data!.getProperty('dtstart')
                let end = data!.getProperty('dtend')
                start.dateonly = chk
                end.dateonly = chk
                data!.setProperty('dtstart', start);
                data!.setProperty('dtend', end);
            });
        }

        let has_repeats_ = this.querySelector('[name="has_repeats"]')
        if (has_repeats_) {
            let has_repeats = has_repeats_ as HTMLInputElement;

            has_repeats.addEventListener('click', () => {
                /* TODO unselecting and reselecting this checkbox deletes all entered data.
                   Cache it somewhere */
                if (has_repeats.checked) {
                    vcal_objects.get(this.uid)!.setProperty('rrule', new RecurrenceRule())
                } else {
                    /* TODO is this a good way to remove a property ? */
                    vcal_objects.get(this.uid)!.setProperty('rrule', undefined)
                }
            })
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


        for (let el of this.querySelectorAll("[data-property]")) {
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
