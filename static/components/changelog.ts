import { makeElement } from '../lib'
import { ComponentVEvent } from './vevent'
import { VEvent } from '../vevent'

export { VEventChangelog }

class VEventChangelog extends ComponentVEvent {

    ul: HTMLElement

    constructor(uid?: string) {
        super(uid);

        this.ul = makeElement('ul');
    }

    connectedCallback() {
        this.replaceChildren(this.ul);
    }

    redraw(data: VEvent) {
        /* TODO only redraw what is needed */
        let children = []
        for (let el of data._changelog) {
            let msg = '';
            switch (el.type) {
                case 'property':
                    msg += `change ${el.name}: `
                    msg += `from "${el.from}" to "${el.to}"`
                    break;
                case 'calendar':
                    if (el.from === null && el.to === null) {
                        msg += '???'
                    } else if (el.from === null) {
                        msg += `set calendar to "${atob(el.to!)}"`
                    } else if (el.to === null) {
                        msg += `Remove calendar "${atob(el.from)}"`
                    } else {
                        msg += `Change calendar from "${atob(el.from)}" to "${atob(el.to)}"`
                    }
                    break;
            }

            children.push(makeElement('li', { textContent: msg }));
        }

        this.ul.replaceChildren(...children)
    }
}
