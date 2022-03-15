export {
    formatters,
}

import { makeElement } from './lib'

let formatters : Map<string, (e : HTMLElement, s : any) => void>;
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

formatters.set('default', (el, d) => {
    let fmt;
    if ((fmt = el.dataset.fmt)) {
        el.textContent = d.format(fmt);
    } else {
        el.textContent = d;
    }
})
