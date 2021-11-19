export { EditRRule }

import { ComponentVEvent } from './vevent'
import { VEvent } from '../vevent'
import { vcal_objects } from '../globals'

import { RecurrenceRule } from '../vevent'

/* <vevent-edit-rrule/> */
class EditRRule extends ComponentVEvent {

    constructor() {
        super();

        let frag = this.template.content.cloneNode(true) as DocumentFragment
        let body = frag.firstElementChild!
        this.replaceChildren(body);
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
