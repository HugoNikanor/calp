export {
    format
}

import { makeElement } from './lib'
import { VEvent } from './vevent'

type formatter = (e: HTMLElement, d: VEvent, s: any) => Promise<void>

declare global {
    interface Window {
        formatters: Map<string, formatter>;
    }
}

let formatters: Map<string, formatter>;
formatters = window.formatters = new Map();

async function format(targetElement: HTMLElement, data: VEvent, key: string): Promise<void> {
    let d = data.getProperty(key);
    if (!d) return
    let formatter = formatters.get(key.toLowerCase());
    if (formatter) {
        try {
            await formatter(targetElement, data, d);
        } catch (error) {
            console.warn('Formatter failed')
            console.warn(error);
            formatters.get('default')!(targetElement, data, d);
        }
    } else {
        formatters.get('default')!(targetElement, data, d);
    }
}

formatters.set('categories', async (el, _, d) => {
    for (let item of d) {
        let q = encodeURIComponent(
            `(member "${item}" (or (prop event (quote CATEGORIES)) (quote ())))`)
        el.appendChild(makeElement('a', {
            textContent: item,
            href: `/search/?q=${q}`,
        }))
    }
})

async function format_time_tag(el: HTMLElement, ev: VEvent, d: any): Promise<void> {
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

formatters.set('default', async (el, _, d) => {
    let fmt;
    if ((fmt = el.dataset.fmt)) {
        el.textContent = d.format(fmt);
    } else {
        el.textContent = d;
    }
})
