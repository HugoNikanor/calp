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
        let fmtVal: string = val;
        if (val instanceof Date) {
            fmtVal = val.format(
                val.dateonly
                    ? '~Y-~m-~d'
                    : '~Y-~m-~dT~H:~M:~S');
        }
        dl.appendChild(makeElement('dd', { innerText: fmtVal }))
    }
    return dl;
}
