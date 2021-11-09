export {
    vcal_objects,
    find_block, find_popup, PopupElement,
    ComponentBlock
}

import { close_popup, toggle_popup } from './popup'
import { VEvent, xml_to_vcal } from './vevent'
import { bind_popup_control } from './dragable'
import { uid, parseDate, gensym, to_local, boolean, makeElement } from './lib'

const vcal_objects: Map<uid, VEvent> = new Map()

interface HasValue {
    value: string
}

function hasValue(obj: any): obj is HasValue {
    return 'value' in obj;
}

/* Root component for all events which content is closely linked to a
@code{VEvent} object

Lacks an accompaning tag, and shouldn't be directly instanciated.
*/
class ComponentVEvent extends HTMLElement {

    template: HTMLTemplateElement
    uid: string

    constructor(uid?: string) {
        super();
        this.template = document.getElementById(this.tagName) as HTMLTemplateElement;

        let real_uid;
        if (this.dataset.uid) uid = this.dataset.uid;
        if (uid) real_uid = uid;

        if (!real_uid) {
            throw `UID required`
        }

        this.uid = real_uid;

        vcal_objects.get(this.uid)?.register(this);

        /* We DON'T have a redraw here in the general case, since the
           HTML rendered server-side should be fine enough for us.
           Those that need a direct rerendering (such as the edit tabs)
           should take care of that some other way */
    }

    connectedCallback() {
        let uid, v;
        if ((uid = this.dataset.uid)) {
            v = vcal_objects.get(uid)
            if (v) this.redraw(v);
        }
    }

    redraw(data: VEvent) {
        // update ourselves from template

        if (!this.template) {
            throw "Something";
        }

        let body = (this.template.content.cloneNode(true) as DocumentFragment).firstElementChild!;

        for (let el of body.getElementsByClassName("bind")) {
            if (!(el instanceof HTMLElement)) continue;
            let p = el.dataset.property!;
            let d, fmt;
            if ((d = data.getProperty(p))) {
                if ((fmt = el.dataset.fmt)) {
                    el.innerHTML = d.format(fmt);
                } else {
                    el.innerHTML = d;
                }
            }
        }

        this.replaceChildren(body);
    }

}


/*
  <vevent-description />
*/
class ComponentDescription extends ComponentVEvent {
    constructor() {
        super();
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

/* <vevent-dl /> */
class VEventDL extends ComponentVEvent {
    redraw(obj: VEvent) {
        let dl = buildDescriptionList(
            Array.from(obj.boundProperties)
                .map(key => [key, obj.getProperty(key)]))
        this.replaceChildren(dl);
    }
}

/* <vevent-edit />
   Edit form for a given VEvent. Used as the edit tab of popups.
*/
class ComponentEdit extends ComponentVEvent {

    firstTime: boolean

    constructor() {
        super();

        this.firstTime = true;
    }

    connectedCallback() {

        /* Edit tab is rendered here. It's left blank server-side, since
           it only makes sense to have something here if we have javascript */

        let data = vcal_objects.get(this.uid)

        if (!data) {
            throw `Data missing for uid ${this.dataset.uid}.`
        }

        this.redraw(data);

        // return;

        for (let el of this.getElementsByClassName("interactive")) {
            // console.log(el);
            el.addEventListener('input', () => {
                let obj = vcal_objects.get(this.uid)
                if (obj === undefined) {
                    throw 'No object with uid ' + this.uid
                }
                if (!(hasValue(el) && el instanceof HTMLElement)) {
                    console.log(el, 'not an HTMLInputElement');
                    return;
                }
                obj.setProperty(
                    el.dataset.property!,
                    el.value)
            });
        }
    }

    redraw(data: VEvent) {
        // update ourselves from template

        if (!this.template) {
            throw "Something";
        }

        let body;
        if (this.firstTime) {
            body = (this.template.content.cloneNode(true) as DocumentFragment).firstElementChild!;
        } else {
            body = this;
        }

        for (let el of body.getElementsByClassName("interactive")) {
            if (!(el instanceof HTMLElement)) continue;
            let p = el.dataset.property!;
            let d: any;
            if ((d = data.getProperty(p))) {
                /*
                  https://stackoverflow.com/questions/57157830/how-can-i-specify-the-sequence-of-running-nested-web-components-constructors
                */
                window.setTimeout(() => {
                    /* NOTE Some specific types might require special formatting
                    here. But due to my custom components implementing custom
                    `.value' procedures, we might not need any special cases
                    here */
                    /* Technically we just want to cast to HTMLElement with
                    value field here, but multiple types implement it
                    sepparately, and no common interface exist */
                    (el as HTMLInputElement).value = d;
                });
            }
        }

        if (this.firstTime) {
            this.replaceChildren(body);
            this.firstTime = false;
        }
    }

}

function find_popup(uid: uid): HTMLElement | null {
    // for (let el of vcal_objects[uid].registered) {
    //     if (el.tagName === 'popup-element') {
    //         return el;
    //     }
    // }
    // throw 'Popup not fonud';
    return document.querySelector(`popup-element[data-uid="${uid}"]`)
}

function find_block(uid: uid): HTMLElement | null {
    let obj = vcal_objects.get(uid)
    if (obj === undefined) {
        return null;
    }
    for (let el of obj.registered) {
        if (el.tagName === 'vevent-block') {
            return el;
        }
    }
    // throw 'Popup not fonud';
    return null;
}

/* <vevent-block />

   A grahpical block in the week view.
*/
class ComponentBlock extends ComponentVEvent {
    constructor(uid?: string) {
        super(uid);

        this.addEventListener('click', () => {
            let uid = this.uid
            let popup = find_popup(uid);
            if (popup === null) throw new Error('no popup for uid ' + uid);
            toggle_popup(popup);
        });
    }

    redraw(data: VEvent) {
        super.redraw(data);

        let p;
        if ((p = data.getProperty('dtstart'))) {
            let c = this.closest('.event-container') as HTMLElement
            let start = parseDate(c.dataset.start!).getTime()
            let end = parseDate(c.dataset.end!).getTime();
            // console.log(p);
            let pp = to_local(p).getTime()
            let result = 100 * (Math.min(end, Math.max(start, pp)) - start) / (end - start) + "%"
            if (c.classList.contains('longevents')) {
                this.style.left = result
            } else {
                this.style.top = result
            }
            // console.log('dtstart', p);
        }
        if ((p = data.getProperty('dtend'))) {
            // console.log('dtend', p);
            let c = this.closest('.event-container') as HTMLElement
            let start = parseDate(c.dataset.start!).getTime()
            let end = parseDate(c.dataset.end!).getTime();
            let pp = to_local(p).getTime()
            let result = 100 - (100 * (Math.min(end, Math.max(start, pp)) - start) / (end - start)) + "%"
            if (c.classList.contains('longevents')) {
                this.style.width = 'unset';
                this.style.right = result;
            } else {
                this.style.height = 'unset';
                this.style.bottom = result;
            }
        }
    }
}

window.addEventListener('load', function() {

    // let json_objects_el = document.getElementById('json-objects');
    let div = document.getElementById('xcal-data')!;
    let vevents = div.firstElementChild!.children;

    for (let vevent of vevents) {
        let ev = xml_to_vcal(vevent);
        vcal_objects.set(ev.getProperty('uid'), ev)
    }

    /*
      - .popup
      - .block
      - .list
     */
    /*
    let vevent_els = document.getElementsByClassName('vevent')
    for (let el of vevent_els) {
        try {
            vcal_objects[el.dataset.uid].register(el);
        } catch {
            console.error("Invalid something, uid = ", el.dataset.uid,
                          "el = ", el
                         );
        }
    }
    */

    customElements.define('vevent-description', ComponentDescription);
    customElements.define('vevent-edit', ComponentEdit);
    customElements.define('vevent-dl', VEventDL);
    customElements.define('vevent-block', ComponentBlock);
})




/* '<date-time-input />' */
class DateTimeInput extends /* HTMLInputElement */ HTMLElement {
    connectedCallback() {
        /* This can be in the constructor for chromium, but NOT firefox...
           Vivaldi 4.3.2439.63 stable
           Mozilla Firefox 94.0.1
        */
        this.innerHTML = '<input type="date" /><input type="time" />'
        // console.log('constructing datetime input')
    }

    static get observedAttributes() {
        return ['dateonly']
    }

    attributeChangedCallback(name: string, _: any, to: any): void {
        // console.log(this, name, boolean(from), boolean(to));
        switch (name) {
            case 'dateonly':
                (this.querySelector('input[type="time"]') as HTMLInputElement)
                    .disabled = boolean(to)
                break;
        }
    }

    get dateonly(): boolean {
        return boolean(this.getAttribute('dateonly'));
    }

    set dateonly(bool: boolean) {
        this.setAttribute('dateonly', "" + bool);
    }

    get valueAsDate(): Date {
        let dt;
        let date = (this.querySelector("input[type='date']") as HTMLInputElement).value;
        if (boolean(this.getAttribute('dateonly'))) {
            dt = parseDate(date);
            dt.type = 'date';
        } else {
            let time = (this.querySelector("input[type='time']") as HTMLInputElement).value;
            dt = parseDate(date + 'T' + time)
            dt.type = 'date-time';
        }
        return dt;
    }

    get value(): string {
        return this.valueAsDate.format("~Y-~m-~dT~H:~M:~S")
    }

    set value(new_value: Date | string) {
        // console.log('Setting date');
        let date, time;
        if (new_value instanceof Date) {
            date = new_value.format("~L~Y-~m-~d");
            time = new_value.format("~L~H:~M:~S");
        } else {
            [date, time] = new_value.split('T')
        }
        (this.querySelector("input[type='date']") as HTMLInputElement).value = date;
        (this.querySelector("input[type='time']") as HTMLInputElement).value = time;
    }

    addEventListener(type: string, proc: ((e: Event) => void)) {
        if (type != 'input') throw "Only input supported";

        (this.querySelector("input[type='date']") as HTMLInputElement)
            .addEventListener(type, proc);
        (this.querySelector("input[type='time']") as HTMLInputElement)
            .addEventListener(type, proc);
    }
}

customElements.define('date-time-input', DateTimeInput /*, { extends: 'input' } */)


function verifySlot(el: Node | null): el is HTMLElement {
    if (el === null) {
        console.error("Element is null");
        return false;
    }
    if (!(el instanceof HTMLElement)) {
        console.error("Node is not an HTMLElement", el);
        return false;
    }
    return true
}


/* <tab-element /> */
class TabElement extends HTMLElement {
    constructor() {
        super();
    }

    connectedCallback() {
        // this.replaceChildren(template.cloneNode(true));
        let template
            = (document.getElementById('tab-template') as HTMLTemplateElement)
                .content
        // const shadowRoot = this.attachShadow({ mode: 'open' })
        //     .appendChild(template.cloneNode(true));
        // console.log(this);
        let label = this.querySelector('[slot="label"]')
        let content = this.querySelector('[slot="content"]')
        if (!verifySlot(label)) throw "Bad label";
        if (!verifySlot(content)) throw "Bad content";

        /* TODO set label hover title somewhere around here */

        this.replaceChildren(template.cloneNode(true));
        this.querySelector('slot[name="label"]')!.replaceWith(label);
        this.querySelector('slot[name="content"]')!.replaceWith(content);
    }
}

function buildDescriptionList(data: [string, any][]): HTMLElement {
    let dl = document.createElement('dl');
    for (let [key, val] of data) {
        dl.appendChild(makeElement('dt', { innerText: key }))
        dl.appendChild(makeElement('dd', { innerText: val }))
    }
    return dl;
}

/* <popup-element /> */
class PopupElement extends ComponentVEvent {

    tabgroup_id: string
    tabcount: number

    constructor(uid?: string) {
        super(uid);

        /* TODO populate remaining */
        // this.id = 'popup' + this.dataset.uid
        this.tabgroup_id = gensym();
        this.tabcount = 0
    }

    redraw() {
        console.warn('IMPLEMENT ME');
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

        let that = this;
        this.getElementsByClassName("calendar-selection")[0]
            .addEventListener('change', function() {
                let uid = (that.closest('[data-uid]') as HTMLElement).dataset.uid!
                let obj = vcal_objects.get(uid)
                // TODO this procedure
                // this.value;
                // event.properties.calendar = this.value;
            });
    }

    addTab(tab: TabElement) {
        let tabgroup = this.getElementsByClassName('tabgroup')![0]!
        tabgroup.append(tab);
        popuplateTab(tab, this.tabgroup_id, this.tabcount)
        this.tabcount += 1
    }
}

window.addEventListener('load', function() {
    customElements.define('popup-element', PopupElement)
    customElements.define('tab-element', TabElement)
});

function wholeday_checkbox(box: HTMLInputElement) {
    box.closest('.timeinput')!
        .querySelectorAll('input[is="date-time"]')
        .forEach((el) => { (el as DateTimeInput).dateonly = box.checked });
}
