import { find_block, find_popup, PopupElement } from './globals'
export {
    event_from_popup, popup_from_event, close_popup,
    close_all_popups, /* VIEW, */open_popup, toggle_popup, activePopup
}

/* TODO rewrite most of this */

/* event component => coresponding popup component */
function event_from_popup(popup: Element): HTMLElement | null {
    // return document.getElementById(popup.id.substr(5))
    let el = popup.closest('[data-uid]')
    if (!el) return null;
    let uid = (el as HTMLElement).dataset.uid
    if (!uid) return null;
    return find_block(uid)
}

/* popup component => coresponding event component */
function popup_from_event(event: Element): HTMLElement | null {
    // return document.getElementById("popup" + event.id);
    // return find_popup(event.closest('[data-uid]').dataset.uid)
    let el = event.closest('[data-uid]')
    if (!el) return null;
    let uid = (el as HTMLElement).dataset.uid
    if (!uid) return null;
    return find_popup(uid)
}

/* hides given popup */
function close_popup(popup: Element): void {
    popup.classList.remove("visible");
}

/* hides all popups */
function close_all_popups() {
    for (let popup of document.querySelectorAll("popup-element.visible")) {
        close_popup(popup);
    }
}

declare let VIEW: 'month' | 'week'

/* open given popup */
function open_popup(popup: HTMLElement) {
    popup.classList.add("visible");
    let element = event_from_popup(popup);
    // let root = document.body;
    let root;
    switch (VIEW) {
        case 'week':
            root = document.getElementsByClassName("days")[0];
            break;
        case 'month':
        default:
            root = document.body;
            break;
    }
    /* start <X, Y> sets offset between top left corner
       of event in calendar and popup. 10, 10 soo old
       event is still visible */
    let offsetX = 10, offsetY = 10;
    while (element !== root && element !== null) {
        offsetX += element.offsetLeft;
        offsetY += element.offsetTop;
        element = element.offsetParent as HTMLElement;
    }
    popup.style.left = offsetX + "px";
    popup.style.top = offsetY + "px";
}

/* toggles open/closed status of popup given by id */
function toggle_popup(popup: HTMLElement) {
    // let popup = document.getElementById(popup_id);
    if (popup.classList.contains("visible")) {
        close_popup(popup);
    } else {
        open_popup(popup);
    }
}

/* Code for managing "selected" popup */
/* Makes the popup last hovered over the selected popup, moving it to
 * the top, and allowing global keyboard bindings to affect it. */

let activePopup: PopupElement | undefined;

for (let popup of document.querySelectorAll('popup-element')) {
    /* TODO possibly only change "active" element after a fraction of
     * a second, for example when moving between tabs */
    popup.addEventListener('mouseover', function() {
        /* This is ever so slightly inefficient,
           but it really dosen't mammet */
        for (let other of
            document.querySelectorAll('popup-element')) {
            /* TODO get this from somewhere */
            /* Currently it's manually copied from the stylesheet */
            ((other as PopupElement).style as any)['z-index'] = 1000;
        }
        ((popup as PopupElement).style as any)['z-index'] += 1;
        activePopup = popup as PopupElement
    });
}

document.addEventListener('keydown', function(event) {
    /* Physical key position, names are what that key would
       be in QWERTY */
    let i = ({
        'KeyQ': 0,
        'KeyW': 1,
        'KeyE': 2,
        'KeyR': 3,
    })[event.code];
    if (i === undefined) return
    if (!activePopup) return;
    let element: HTMLLabelElement | undefined = activePopup.querySelectorAll(".tab > label")[i] as HTMLLabelElement;
    if (!element) return;
    element.click();
});

/* END Code for managing "selected" popup */
