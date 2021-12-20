export { ComponentDescription }

import { VEvent } from '../vevent'
import { ComponentVEvent } from './vevent'
import { makeElement } from '../lib'

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
            let d, fmt;
            if ((d = data.getProperty(p))) {
                switch (p.toLowerCase()) {
                    case 'categories':
                        for (let item of d) {
                            let q = encodeURIComponent(
                                `(member "${item}" (or (prop event (quote CATEGORIES)) (quote ())))`)
                            el.appendChild(makeElement('a', {
                                textContent: item,
                                href: `/search/?q=${q}`,
                            }))
                        }
                        break;
                    default:
                        if ((fmt = el.dataset.fmt)) {
                            el.textContent = d.format(fmt);
                        } else {
                            el.textContent = d;
                        }
                }
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
