export {
    formatters,
}

import { makeElement } from './lib'
import { VEvent } from './vevent'

type formatter = (e: HTMLElement, d: VEvent, s: any) => void

declare global {
    interface Window {
        formatters: Map<string, formatter>;
    }
}

let formatters: Map<string, formatter>;
formatters = window.formatters = new Map();


formatters.set('categories', (el, _, d) => {
    for (let item of d) {
        let q = encodeURIComponent(
            `(member "${item}" (or (prop event (quote CATEGORIES)) (quote ())))`)
        el.appendChild(makeElement('a', {
            textContent: item,
            href: `/search/?q=${q}`,
        }))
    }
})

function format_time_tag(el: HTMLElement, ev: VEvent, d: any): void {
    if (el instanceof HTMLTimeElement) {
        if (d instanceof Date) {
            let fmt = '';
            if (!d.utc) {
                fmt += '~L';
            }
            fmt += '~Y-~m-~d'
            if (!d.dateonly) {
                fmt += 'T~H:~M:~S'
            }
            el.dateTime = d.format(fmt);
        }
    }

    formatters.get('default')!(el, ev, d);
}

formatters.set('dtstart', format_time_tag)
formatters.set('dtend', format_time_tag)

formatters.set('default', (el, _, d) => {
    let fmt;
    if ((fmt = el.dataset.fmt)) {
        el.textContent = d.format(fmt);
    } else {
        el.textContent = d;
    }
})
