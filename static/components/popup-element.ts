export { PopupElement }

import { gensym } from '../lib'
import { VEvent } from '../vevent'
import { bind_popup_control } from '../dragable'
import { close_popup, event_from_popup } from '../popup'
import { vcal_objects } from '../globals'

import { ComponentVEvent } from './vevent'

import { remove_event } from '../server_connect'

/* <popup-element /> */
class PopupElement extends ComponentVEvent {

    tabgroup_id: string
    tabcount: number

    isVisible: boolean = false;

    constructor(uid?: string) {
        super(uid);

        /* TODO populate remaining (??) */

        this.tabgroup_id = gensym();
        this.tabcount = 0

        let obj = vcal_objects.get(this.uid);
        if (obj && obj.calendar) {
            this.dataset.calendar = obj.calendar;
        }
    }

    redraw(data: VEvent) {
        if (data.calendar) {
            this.dataset.calendar = data.calendar;
        }

        /* TODO is there any case where we want to propagate the draw to any of
           our tabs? or are all our tabs independent? */
    }

    connectedCallback() {
        let template: HTMLTemplateElement = document.getElementById('popup-template') as HTMLTemplateElement
        let body = (template.content.cloneNode(true) as DocumentFragment).firstElementChild!;

        let uid = this.uid;

        window.setTimeout(() => {

            /* tab change button */
            let tabs = this.querySelectorAll('[role="tab"]')
            /* list of all tabs */
            // let tablist = this.querySelector('[role="tablist"]')!

            tabs.forEach(tab => {
                tab.addEventListener('click', () => {

                    /* hide all tab panels */
                    for (let tabcontent of this.querySelectorAll('[role="tabpanel"]')) {
                        tabcontent.setAttribute('hidden', 'true');
                    }
                    /* unselect all (selected) tab handles */
                    for (let item of this.querySelectorAll('[aria-selected="true"]')) {
                        item.setAttribute('aria-selected', 'false');
                    }
                    /* re-select ourselves */
                    tab.setAttribute('aria-selected', 'true');

                    /* unhide our target tab */
                    this.querySelector('#' + tab.getAttribute('aria-controls'))!
                        .removeAttribute('hidden')
                });
            });

            /* tab contents */
            let tabcontents = this.querySelectorAll('[role="tabpanel"]')

            for (let i = 0; i < tabs.length; i++) {
                let n = i + this.tabcount;
                this.tabgroup_id
                let tab = tabs[n];
                let con = tabcontents[n];

                let a = `${this.tabgroup_id}-tab-${n}`
                let b = `${this.tabgroup_id}-con-${n}`

                tab.id = a;
                con.setAttribute('aria-labeledby', a);

                con.id = b;
                tab.setAttribute('aria-controls', b);

            }
            this.tabcount += tabs.length

        });
        /* end tabs */

        /* nav bar */
        let nav = body.getElementsByClassName("popup-control")[0] as HTMLElement;
        bind_popup_control(nav);

        let close_btn = body.querySelector('.popup-control .close-button') as HTMLButtonElement
        close_btn.addEventListener('click', () => close_popup(this));

        let remove_btn = body.querySelector('.popup-control .remove-button') as HTMLButtonElement
        remove_btn.addEventListener('click', () => remove_event(uid));
        /* end nav bar */

        this.replaceChildren(body);
    }

    static get observedAttributes() {
        return ['visible'];
    }

    get visible(): boolean {
        return this.isVisible;
    }

    set visible(isVisible: boolean) {
        this.isVisible = isVisible;
        if (this.isVisible) {
            this.classList.add('visible');
        } else {
            this.classList.remove('visible');
        }

        let root;
        switch (window.VIEW) {
            case 'week':
                root = document.getElementsByClassName("days")[0];
                break;
            case 'month':
            default:
                root = document.body;
                break;
        }

        let element = event_from_popup(this) as HTMLElement;
        /* start <X, Y> sets offset between top left corner
           of event in calendar and popup. 10, 10 soo old
           event is still visible */
        let offsetX = 10, offsetY = 10;
        while (element !== root && element !== null) {
            offsetX += element.offsetLeft;
            offsetY += element.offsetTop;
            element = element.offsetParent as HTMLElement;
        }
        this.style.left = offsetX + "px";
        this.style.top = offsetY + "px";
    }
}
