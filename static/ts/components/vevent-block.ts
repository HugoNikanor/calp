/**
 * `<vevent-block />`
 *
 * A block in our graphical view.
 *
 * Unique in that it works quite differently between the week and month view.
 *
 * @category Web Components
 * @mergeTarget components
 * @module
 */
export { ComponentBlock }

import { ComponentVEvent } from './vevent'
import { VEvent } from '../vevent'
import { parseDate, to_local } from '../lib'


/* <vevent-block />

   A grahpical block in the week view.
*/
class ComponentBlock extends ComponentVEvent {
    constructor(uid?: string) {
        super(uid);

        if (!this.template) {
            throw 'vevent-block template required';
        }

        this.addEventListener('click', () => {
            let uid = this.uid
            /* TODO is it better to find the popup through a query selector, or
               by looking through all registered components of a VEvent? */
            let popup = document.querySelector(`popup-element[data-uid="${uid}"]`)
            if (popup === null) throw new Error('no popup for uid ' + uid);
            popup.toggleAttribute('visible');
        });
    }

    redraw(data: VEvent) {
        let body = (this.template!.content.cloneNode(true) as DocumentFragment).firstElementChild!;

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
            } else switch (p.toLowerCase()) {
                /* We lack that property, but might want to set a default here */
                case 'summary':
                    el.textContent = 'Ny händelse'
                    break;
            }
        }

        this.replaceChildren(body);

        /* -------------------------------------------------- */

        if (window.VIEW === 'week') {
            let p;
            if ((p = data.getProperty('dtstart'))) {
                let c = this.closest('.event-container') as HTMLElement
                let start = parseDate(c.dataset.start!).getTime()
                let end = parseDate(c.dataset.end!).getTime();
                // console.log(p);
                let pp = to_local(p).getTime()
                let result = 100 * (Math.min(end, Math.max(start, pp)) - start) / (end - start) + "%"
                if (c.classList.contains('longevents')) {
                    this.style.left = result
                } else {
                    this.style.top = result
                }
                // console.log('dtstart', p);
            }
            if ((p = data.getProperty('dtend'))) {
                // console.log('dtend', p);
                let c = this.closest('.event-container') as HTMLElement
                let start = parseDate(c.dataset.start!).getTime()
                let end = parseDate(c.dataset.end!).getTime();
                let pp = to_local(p).getTime()
                let result = 100 - (100 * (Math.min(end, Math.max(start, pp)) - start) / (end - start)) + "%"
                if (c.classList.contains('longevents')) {
                    this.style.width = 'unset';
                    this.style.right = result;
                } else {
                    this.style.height = 'unset';
                    this.style.bottom = result;
                }
            }
        }

        if (data.calendar) {
            this.dataset.calendar = data.calendar;
        }

        if (data.getProperty('rrule') !== undefined) {
            let rep = this.getElementsByClassName('repeating')
            if (rep.length !== 0) {
                (rep[0] as HTMLElement).innerText = '↺'
            }
        }
    }
}
