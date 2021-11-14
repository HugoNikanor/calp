export { ComponentBlock }

import { ComponentVEvent } from './vevent'
import { VEvent } from '../vevent'
import { toggle_popup, find_popup } from '../popup'
import { parseDate, to_local } from '../lib'


/* <vevent-block />

   A grahpical block in the week view.
*/
class ComponentBlock extends ComponentVEvent {
    constructor(uid?: string) {
        super(uid);

        this.addEventListener('click', () => {
            let uid = this.uid
            let popup = find_popup(uid);
            if (popup === null) throw new Error('no popup for uid ' + uid);
            toggle_popup(popup);
        });
    }

    redraw(data: VEvent) {
        super.redraw(data);

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

        if (data.calendar) {
            this.dataset.calendar = data.calendar;
        }
    }
}
