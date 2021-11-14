export { PopupElement }

import { gensym } from '../lib'
import { VEvent } from '../vevent'
import { bind_popup_control } from '../dragable'
import { close_popup, event_from_popup } from '../popup'

import { ComponentVEvent } from './vevent'
import { TabElement } from './tab-element'

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
    }

    redraw(data: VEvent) {
        // console.warn('IMPLEMENT ME');

        console.log('popup', data.calendar);
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
        // console.log(uid);

        body.getElementsByClassName('populate-with-uid')
            .forEach((e) => e.setAttribute('data-uid', uid));

        /* tabs */
        // for (let tab of body.querySelectorAll(".tabgroup .tab")) {
        // }
        window.setTimeout(() => {
            // let tabs = this.querySelector('tab-element')!
            //     .shadowRoot!
            //     .querySelectorAll('label')
            // console.log(tabs);
            // console.log(this.getElementsByTagName('tab-element'))
            for (let tab of this.getElementsByTagName('tab-element')) {
                // console.log(tab_container);
                // let tab = tab_container.shadowRoot!;
                // tab.documentElement.style.setProperty('--i', i);
                popuplateTab(tab as TabElement, this.tabgroup_id, this.tabcount)
                this.tabcount += 1
            }
            (this.querySelector('tab-element label') as HTMLInputElement).click()
        });
        /* end tabs */

        /* nav bar */
        let nav = body.getElementsByClassName("popup-control")[0] as HTMLElement;
        bind_popup_control(nav);

        let btn = body.querySelector('.popup-control .close-tooltip') as HTMLButtonElement
        btn.addEventListener('click', () => close_popup(this));
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

    addTab(tab: TabElement) {
        let tabgroup = this.getElementsByClassName('tabgroup')![0]!
        tabgroup.append(tab);
        popuplateTab(tab, this.tabgroup_id, this.tabcount)
        this.tabcount += 1
    }
}

function popuplateTab(tab: HTMLElement, tabgroup: string, index: number) {
    // console.log(tab);
    let new_id = gensym();
    let input = tab.querySelector('input[type="radio"]') as HTMLInputElement;
    let label = tab.querySelector("label")!
    tab.style.setProperty('--tab-index', '' + index);
    /* TODO this throws a number of errors, but somehow still works...? */
    if (input !== null) {
        input.name = tabgroup
        input.id = new_id;
    }
    if (label !== null) {
        label.setAttribute('for', new_id);
    }
}
