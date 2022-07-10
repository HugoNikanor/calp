export { ComponentDescription }

import { VEvent } from '../vevent'
import { ComponentVEvent } from './vevent'
import { format } from '../formatters'

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
            format(el, data, el.dataset.property!);
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
