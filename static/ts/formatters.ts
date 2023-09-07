/**
 * Formatting procedures used by some components.
 *
 * // TODO can we have a backref of every node containing `{@link formatters-proc}`?
 *
 * {@label FORMATTERS}
 *
 * Each procedure takes three arguments. The HTML-element which contents
 * should be replaced, the VEvent containing all data, and the target
 * value, as returned by {@linkcode vevent.VEvent.getProperty}.
 *
 * Also bound to the window object.
 *
 * @module
 */

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

/**
 * Checks if a specific formatter exists for the given key, and executes it.
 * Defaults to 'default', and also runs that if the regular formatter throws.
 */
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
