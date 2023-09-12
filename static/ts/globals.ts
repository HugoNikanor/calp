/**
 * Different variables and values which for different reasons needs to be
 * global. Window Value's are those that are bound to the `window`
 * context in JavaScript, so is really always available, no opt out.
 * @module
 */

export {
    find_block,
    vcal_objects, event_calendar_mapping
}

import { VEvent } from './vevent'
import { uid } from './types'
import { ComponentBlock } from './components/vevent-block'

import { v4 as uuid } from 'uuid'
import { setup_popup_element } from './components/popup-element'

/**
 * All VEvent objects on current page, indexed by their unique identifiers.
 *
 * A global object store.
 *
 * Also bound to the window object for easy access.
 */
const vcal_objects: Map<uid, VEvent> = new Map;

/**
 * Mapping from VEvent unique identifier, to name of its calendar. Should
 * probably not be global, so refrain from using it.
 */
const event_calendar_mapping: Map<uid, string> = new Map;

declare global {
    interface Window {
        vcal_objects: Map<uid, VEvent>;
        /**
         * How the calendar is currently formatted. Should be set by the backend
         * through a simple `script`-tag.
         */
        VIEW: 'month' | 'week';
        /**
         * However editing of events is enabled or not.
         * Should be set by the backend through a simple `script`-tag.
         */
        EDIT_MODE: boolean;
        /**
         * Name of the calendar to assume when creating new events.
         * Should be set by the backend through a simple `script`-tag.
         */
        default_calendar: string;

        addNewEvent(): void;
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

/**
   Find the calendar block in the inline view containing the VEvent identified
   by the uid
*/
function find_block(uid: uid): ComponentBlock | null {
    let obj = vcal_objects.get(uid)
    if (obj === undefined) {
        return null;
    }
    for (let el of obj.registered) {
        if (el instanceof ComponentBlock) {
            return el;
        }
    }
    // throw 'Popup not fonud';
    return null;
}
