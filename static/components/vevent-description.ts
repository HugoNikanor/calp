export { ComponentDescription }

import { VEvent } from '../vevent'
import { ComponentVEvent } from './vevent'
import { formatters } from '../formatters'

/*
  <vevent-description />
*/
class ComponentDescription extends ComponentVEvent {

    constructor(uid?: string) {
        super(uid);
        if (!this.template) {
            throw 'vevent-description template required';
        }
    }

    redraw(data: VEvent) {
        // update ourselves from template

        let body = (this.template!.content.cloneNode(true) as DocumentFragment).firstElementChild!;

        for (let el of body.querySelectorAll('[data-property]')) {
            if (!(el instanceof HTMLElement)) continue;
            let p = el.dataset.property!;
            let d;
            if ((d = data.getProperty(p))) {
                let key = p.toLowerCase();
                let f = formatters.get(key);
                if (f) f(el, data, d);
                else window.formatters.get('default')!(el, data, d);
            }
        }

        let repeating = body.getElementsByClassName('repeating')[0] as HTMLElement
        if (data.getProperty('rrule')) {
            repeating.classList.remove('hidden');
        } else {
            repeating.classList.add('hidden');
        }

        this.replaceChildren(body);
    }
}
