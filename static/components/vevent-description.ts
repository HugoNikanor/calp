export { ComponentDescription }

import { VEvent } from '../vevent'
import { ComponentVEvent } from './vevent'

/*
  <vevent-description />
*/
class ComponentDescription extends ComponentVEvent {
    constructor() {
        super();
    }

    redraw(data: VEvent) {
        // update ourselves from template

        let body = (this.template.content.cloneNode(true) as DocumentFragment).firstElementChild!;

        for (let el of body.querySelectorAll('[data-property]')) {
            if (!(el instanceof HTMLElement)) continue;
            let p = el.dataset.property!;
            let d, fmt;
            if ((d = data.getProperty(p))) {
                if ((fmt = el.dataset.fmt)) {
                    el.textContent = d.format(fmt);
                } else {
                    el.textContent = d;
                }
            }
        }

        this.replaceChildren(body);
    }
}
