export { VEventDL }

import { ComponentVEvent } from './vevent'
import { VEvent } from '../vevent'
import { makeElement } from '../lib'

import { RecurrenceRule } from '../vevent'

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
        dl.appendChild(makeElement('dt', { textContent: key }))
        let fmtVal: string = val;
        if (val instanceof Date) {
            fmtVal = val.format(
                val.dateonly
                    ? '~Y-~m-~d'
                    : '~Y-~m-~dT~H:~M:~S');
        } else if (val instanceof RecurrenceRule) {
            fmtVal = JSON.stringify(val.to_jcal())
        }
        dl.appendChild(makeElement('dd', { textContent: fmtVal }))
    }
    return dl;
}
