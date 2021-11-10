export {
    find_block,
    VIEW, EDIT_MODE,
    vcal_objects, event_calendar_mapping
}

import { VEvent } from './vevent'
import { uid } from './types'

const vcal_objects: Map<uid, VEvent> = new Map;
const event_calendar_mapping: Map<uid, string> = new Map;

declare global {
    interface Window {
        vcal_objects: Map<uid, VEvent>;
    }
}
window.vcal_objects = vcal_objects;

declare let VIEW: 'month' | 'week'
declare let EDIT_MODE: boolean


function find_block(uid: uid): HTMLElement | null {
    let obj = vcal_objects.get(uid)
    if (obj === undefined) {
        return null;
    }
    for (let el of obj.registered) {
        if (el.tagName === 'vevent-block') {
            return el;
        }
    }
    // throw 'Popup not fonud';
    return null;
}








/*
function wholeday_checkbox(box: HTMLInputElement) {
    box.closest('.timeinput')!
        .querySelectorAll('input[is="date-time"]')
        .forEach((el) => { (el as DateTimeInput).dateonly = box.checked });
}
*/
