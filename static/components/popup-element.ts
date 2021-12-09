export { PopupElement }

import { VEvent } from '../vevent'
import { bind_popup_control } from '../dragable'
import { find_block, vcal_objects } from '../globals'

import { ComponentVEvent } from './vevent'

import { remove_event } from '../server_connect'

/* <popup-element /> */
class PopupElement extends ComponentVEvent {

    /* The popup which is the "selected" popup.
    /* Makes the popup last hovered over the selected popup, moving it to
     * the top, and allowing global keyboard bindings to affect it. */
    static activePopup: PopupElement | null = null;

    constructor(uid?: string) {
        super(uid);

        /* TODO populate remaining (??) */

        let obj = vcal_objects.get(this.uid);
        if (obj && obj.calendar) {
            this.dataset.calendar = obj.calendar;
        }

        /* Makes us the active popup */
        this.addEventListener('mouseover', () => {
            if (PopupElement.activePopup) {
                PopupElement.activePopup.removeAttribute('active');
            }
            PopupElement.activePopup = this;
            this.setAttribute('active', 'active');
        })
    }

    redraw(data: VEvent) {
        if (data.calendar) {
            /* The CSS has hooks on [data-calendar], meaning that this can
               (and will) change stuff */
            this.dataset.calendar = data.calendar;
        }

    }

    connectedCallback() {
        let template = document.getElementById('popup-template') as HTMLTemplateElement
        let body = (template.content.cloneNode(true) as DocumentFragment).firstElementChild!;

        let uid = this.uid;

        /* nav bar */
        let nav = body.getElementsByClassName("popup-control")[0] as HTMLElement;
        bind_popup_control(nav);

        let close_btn = body.querySelector('.popup-control .close-button') as HTMLButtonElement
        close_btn.addEventListener('click', () => this.visible = false);

        let maximize_btn = body.querySelector('.popup-control .maximize-button') as HTMLButtonElement
        maximize_btn.addEventListener('click', () => {
            /* TODO this assumes that popups are direct decendant of their parent,
               which they really ought to be */
            let parent = this.parentElement!;
            let el = this.firstElementChild as HTMLElement
            /* TODO offsetParent.scrollLeft places us "fullscreen" according to the currently
               scrolled viewport. But is this the correct way to do it? How does it work for
               month views */
            this.style.left = `${this.offsetParent!.scrollLeft + 10}px`;
            this.style.top = '10px';
            /* 5ex is width of tab labels */
            el.style.width = `calc(${parent.clientWidth - 20}px - 5ex)`
            el.style.height = `${parent.clientHeight - 20}px`
        });

        let remove_btn = body.querySelector('.popup-control .remove-button') as HTMLButtonElement
        remove_btn.addEventListener('click', () => remove_event(uid));
        /* end nav bar */

        this.replaceChildren(body);
    }

    static get observedAttributes() {
        return ['visible'];
    }

    attributeChangedCallback(name: string, oldValue?: string, newValue?: string) {
        switch (name) {
            case 'visible':
                this.onVisibilityChange()
                break;
        }
    }

    get visible(): boolean {
        return this.hasAttribute('visible');
    }

    set visible(isVisible: boolean) {
        if (isVisible) {
            this.setAttribute('visible', 'visible');
        } else {
            this.removeAttribute('visible');
        }
    }

    private onVisibilityChange() {

        /* TODO better way to find root */
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

        let element = find_block(this.uid) as HTMLElement | null
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

        /* Reset width and height to initial, to save user if they have resized
        it to something weird */
        let el = this.firstElementChild as HTMLElement;
        el.style.removeProperty('width');
        el.style.removeProperty('height');
    }
}
