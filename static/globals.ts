export {
    find_block,
    vcal_objects, event_calendar_mapping
}

import { VEvent } from './vevent'
import { uid } from './types'
import { ComponentBlock } from './components/vevent-block'

import { v4 as uuid } from 'uuid'
import { setup_popup_element } from './components/popup-element'

const vcal_objects: Map<uid, VEvent> = new Map;
const event_calendar_mapping: Map<uid, string> = new Map;

declare global {
    interface Window {
        vcal_objects: Map<uid, VEvent>;
        VIEW: 'month' | 'week';
        EDIT_MODE: boolean;
        default_calendar: string;

        addNewEvent: ((e: any) => void);
    }
}
window.vcal_objects = vcal_objects;


window.addNewEvent = () => {
    let ev = new VEvent();
    let uid = uuid()
    let now = new Date()
    /* Round seconds to 0, since time inputs wants exact seconds */
    now.setUTCSeconds(0);
    ev.setProperties([
        ['uid', uid],
        ['dtstart', now, 'date-time'],
        ['dtend', new Date(now.getTime() + 3600 * 1000), 'date-time'],
    ])
    ev.calendar = window.default_calendar;

    vcal_objects.set(uid, ev);

    let popup = setup_popup_element(ev);
    popup.maximize();
}

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
