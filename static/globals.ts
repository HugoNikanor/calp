export {
    find_block,
    vcal_objects, event_calendar_mapping
}

import { VEvent } from './vevent'
import { uid } from './types'
import { ComponentBlock } from './components/vevent-block'

const vcal_objects: Map<uid, VEvent> = new Map;
const event_calendar_mapping: Map<uid, string> = new Map;

declare global {
    interface Window {
        vcal_objects: Map<uid, VEvent>;
        VIEW: 'month' | 'week';
        EDIT_MODE: boolean;
        default_calendar: string;
    }
}
window.vcal_objects = vcal_objects;

function find_block(uid: uid): ComponentBlock | null {
    let obj = vcal_objects.get(uid)
    if (obj === undefined) {
        return null;
    }
    for (let el of obj.registered) {
        if (el.tagName === 'vevent-block') {
            return el as ComponentBlock;
        }
    }
    // throw 'Popup not fonud';
    return null;
}
