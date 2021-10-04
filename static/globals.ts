"use strict";

const vcal_objects: Map<uid, VEvent> = new Map()

class ComponentVEvent extends HTMLElement {

    template: HTMLTemplateElement

    constructor() {
        super();
        this.template = document.getElementById(this.tagName) as HTMLTemplateElement;

        let uid;
        if ((uid = this.dataset.uid)) {
            vcal_objects.get(uid)?.register(this);
        }

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

class ComponentDescription extends ComponentVEvent {
    constructor() {
        super();
    }

}

class ComponentEdit extends ComponentVEvent {

    firstTime: boolean
    uid: string

    constructor() {
        super();

        this.firstTime = true;

        if (this.dataset.uid === undefined) {
            throw "data-uid must be set"
        } else {
            this.uid = this.dataset.uid;
        }
    }

    connectedCallback() {

        /* Edit tab is rendered here. It's left blank server-side, since
           it only makes sense to have something here if we have javascript */

        let data = vcal_objects.get(this.uid)

        if (!data) {
            throw `Data missing for uid ${this.dataset.uid}.`
        }

        this.redraw(data);

        return;

        for (let el of this.getElementsByClassName("interactive")) {
            el.addEventListener('input', () => {
                let obj = vcal_objects.get(this.uid)
                if (obj === undefined) {
                    throw 'No object with uid ' + this.uid
                }
                if (!(el instanceof HTMLInputElement)) return;
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
            if (!(el instanceof HTMLInputElement)) continue;
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
                    console.log(el, d);
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
    return document.querySelector(`popup-element[data-uid="${uid}"]`) as HTMLElement
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

class ComponentBlock extends ComponentVEvent {
    constructor() {
        super();

        this.addEventListener('click', () => {
            let uid = this.dataset.uid
            if (uid === undefined) throw new Error('UID missing from' + this)
            let popup = find_popup(uid);
            if (popup === null) throw new Error('no popup for uid ' + uid);
            toggle_popup(popup);
        });
    }

    redraw(data: VEvent) {
        super.redraw(data);

        let p;
        if ((p = data.getProperty('dtstart'))) {
            this.style.top = date_to_percent(to_local(p)) + "%";
            // console.log('dtstart', p);
        }
        if ((p = data.getProperty('dtend'))) {
            this.style.height = 'unset';
            // console.log('dtend', p);
            this.style.bottom = (100 - date_to_percent(to_local(p))) + "%";
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
    customElements.define('vevent-block', ComponentBlock);
})




class DateTimeInput extends /* HTMLInputElement */ HTMLElement {
    constructor() {
        super();
        this.innerHTML = '<input type="date" /><input type="time" />'
        console.log('constructing datetime input')
    }

    static get observedAttributes() {
        return ['dateonly']
    }

    attributeChangedCallback(name: string, from: any, to: any) {
        console.log(this, name, boolean(from), boolean(to));
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
        console.log('Setting date');
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

class PopupElement extends HTMLElement {
    constructor() {
        super();

        /* TODO populate remaining */
        // this.id = 'popup' + this.dataset.uid
    }

    redraw() {
        console.log('IMPLEMENT ME');
    }

    connectedCallback() {
        let template: HTMLTemplateElement = document.getElementById('popup-template') as HTMLTemplateElement
        let body = (template.content.cloneNode(true) as DocumentFragment).firstElementChild!;

        if (this.dataset.uid === null) {
            throw 'UID is required'
        }
        let uid = this.dataset.uid!
        // console.log(uid);

        body.getElementsByClassName('populate-with-uid')
            .forEach((e) => e.setAttribute('data-uid', uid));

        /* tabs */
        let tabgroup_id = gensym();
        for (let tab of body.querySelectorAll(".tabgroup .tab")) {
            let new_id = gensym();
            let input = tab.querySelector("input")!;
            input.id = new_id;
            input.name = tabgroup_id;
            tab.querySelector("label")!.setAttribute('for', new_id);
        }
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
}

window.addEventListener('load', function() {
    customElements.define('popup-element', PopupElement)
});

function wholeday_checkbox(box: HTMLInputElement) {
    box.closest('.timeinput')!
        .querySelectorAll('input[is="date-time"]')
        .forEach((el) => { (el as DateTimeInput).dateonly = box.checked });
}
