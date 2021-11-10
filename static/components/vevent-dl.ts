export { VEventDL }

import { ComponentVEvent } from './vevent'
import { VEvent } from '../vevent'
import { makeElement } from '../lib'

/* <vevent-dl /> */
class VEventDL extends ComponentVEvent {
    redraw(obj: VEvent) {
        let dl = buildDescriptionList(
            Array.from(obj.boundProperties)
                .map(key => [key, obj.getProperty(key)]))
        this.replaceChildren(dl);
    }
}

function buildDescriptionList(data: [string, any][]): HTMLElement {
    let dl = document.createElement('dl');
    for (let [key, val] of data) {
        dl.appendChild(makeElement('dt', { innerText: key }))
        dl.appendChild(makeElement('dd', { innerText: val }))
    }
    return dl;
}
