export { EditRRule }

import { ComponentVEvent } from './vevent'
import { VEvent } from '../vevent'
import { vcal_objects } from '../globals'

import { RecurrenceRule } from '../vevent'

/* <vevent-edit-rrule/>
   Tab for editing the recurrence rule of a component
*/
class EditRRule extends ComponentVEvent {

    constructor(uid?: string) {
        super(uid);

        if (!this.template) {
            throw 'vevent-edit-rrule template required';
        }

        let frag = this.template.content.cloneNode(true) as DocumentFragment
        let body = frag.firstElementChild!
        this.replaceChildren(body);

        for (let el of this.querySelectorAll('[name]')) {
            el.addEventListener('input', () => {
                // console.log(this);
                let data = vcal_objects.get(this.uid)!;
                let rrule = data.getProperty('rrule')
                if (!rrule) {
                    console.warn('RRUle missing from object');
                    return;
                }
                rrule = rrule as RecurrenceRule

                console.log(el.getAttribute('name'), (el as any).value);
                rrule[el.getAttribute('name')!] = (el as any).value;
                data.setProperty('rrule', rrule);

            });
        }
    }

    connectedCallback() {
        this.redraw(vcal_objects.get(this.uid)!)
    }

    redraw(data: VEvent) {

        let rrule = data.getProperty('rrule')
        if (!rrule) return;
        rrule = rrule as RecurrenceRule

        for (let el of this.querySelectorAll('[name]')) {

            /*
              el ought to be one of the tag types:
                <input/>, <input-list/>, <select/>, and <date-time-input/>
              Which all have `name` and `value` fields, allowing the code
              below to work.
            */

            let name = el.getAttribute('name')
            if (!name) {
                console.warn(`Input without name, ${el}`)
                continue
            }

            let value: any = rrule[name];
            if (value)
                (el as any).value = value;
        }
    }

}
