export {
    formatters,
}

import { makeElement } from './lib'

declare global {
    interface Window {
        formatters: Map<string, (e: HTMLElement, s: any) => void>;
    }
}

let formatters: Map<string, (e: HTMLElement, s: any) => void>;
formatters = window.formatters = new Map();


formatters.set('categories', (el, d) => {
    for (let item of d) {
        let q = encodeURIComponent(
            `(member "${item}" (or (prop event (quote CATEGORIES)) (quote ())))`)
        el.appendChild(makeElement('a', {
            textContent: item,
            href: `/search/?q=${q}`,
        }))
    }
})

function format_time_tag(el: HTMLElement, d: any): void {
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

    formatters.get('default')!(el, d);
}

formatters.set('dtstart', format_time_tag)
formatters.set('dtend', format_time_tag)

formatters.set('default', (el, d) => {
    let fmt;
    if ((fmt = el.dataset.fmt)) {
        el.textContent = d.format(fmt);
    } else {
        el.textContent = d;
    }
})
