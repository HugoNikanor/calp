"use strict";

const vcal_objects = {};

class ComponentVEvent extends HTMLElement {
    constructor () {
        super ();
        this.template = document.getElementById(this.tagName);

        /* We DON'T have a redraw here in the general case, since the
           HTML rendered server-side should be fine enough for us.
           Those that need a direct rerendering (such as the edit tabs)
           should take care of that some other way */
    }

    connectedCallback () {
        let uid;
        if ((uid = this.dataset.uid)) {
            this.redraw (vcal_objects[uid]);
        }
    }

    redraw (data) {
        // update ourselves from template

        if (! this.template) {
            throw "Something";
        }

        let body = this.template.content.cloneNode(true).firstElementChild;

        for (let el of body.getElementsByClassName("bind")) {
            let p = el.dataset.property;
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
    constructor () {
        super() ;
    }

}

class ComponentEdit extends ComponentVEvent {
    constructor () {
        super();

        this.firstTime = true;
    }

    connectedCallback() {

        /* Edit tab is rendered here. It's left blank server-side, since
           it only makes sense to have something here if we have javascript */

        let data = vcal_objects[this.dataset.uid]

        if (! data) {
            throw `Data missing for uid ${this.dataset.uid}.`
        }

        this.redraw(data);

        for (let el of this.getElementsByClassName("interactive")) {
            el.addEventListener('input', () => {
                vcal_objects[this.dataset.uid].setProperty(
                    el.dataset.property,
                    el.value)
            });
        }
    }

    redraw (data) {
        // update ourselves from template

        if (! this.template) {
            throw "Something";
        }

        let body;
        if (this.firstTime) {
            body = this.template.content.cloneNode(true).firstElementChild;
        } else {
            body = this;
        }

        for (let el of body.getElementsByClassName("interactive")) {
            let p = el.dataset.property;
            let d;
            if ((d = data.getProperty(p))) {
                /*
                  https://stackoverflow.com/questions/57157830/how-can-i-specify-the-sequence-of-running-nested-web-components-constructors
                */
                window.setTimeout (() => {
                    /* NOTE Some specific types might require special formatting
                    here. But due to my custom components implementing custom
                    `.value' procedures, we might not need any special cases
                    here */
                    el.value = d;
                });
            }
        }

        if (this.firstTime) {
            this.replaceChildren(body);
            this.firstTime = false;
        }
    }

}

function find_popup (uid) {
    for (let el of vcal_objects[uid].registered) {
        if (el.tagName === 'popup-element') {
            return el;
        }
    }
    throw 'Popup not fonud';
}

function find_block (uid) {
    for (let el of vcal_objects[uid].registered) {
        if (el.tagName === 'vevent-block') {
            return el;
        }
    }
    throw 'Popup not fonud';
}

class ComponentBlock extends ComponentVEvent {
    constructor () {
        super();

        this.addEventListener('click', () => {
            toggle_popup(find_popup(this.dataset.uid));
        });
    }

    redraw (data) {
        super.redraw(data);

        let p;
        if ((p = data.getProperty('dtstart'))) {
            this.style.top = date_to_percent(to_local(p), 1) + "%";
            // console.log('dtstart', p);
        }
        if ((p = data.getProperty('dtend'))) {
            this.style.height = 'unset';
            // console.log('dtend', p);
            this.style.bottom = (100 - date_to_percent(to_local(p), 1)) + "%";
        }
    }
}

window.addEventListener('load', function () {

    // let json_objects_el = document.getElementById('json-objects');
    let div = document.getElementById('xcal-data');
    let vevents = div.firstElementChild.childNodes;

    for (let vevent of vevents) {
        let ev = xml_to_vcal(vevent);
        vcal_objects[ev.getProperty('uid')] = ev
    }

    /*
      - .popup
      - .block
      - .list
     */
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

    customElements.define('vevent-description', ComponentDescription);
    customElements.define('vevent-edit', ComponentEdit);
    customElements.define('vevent-block', ComponentBlock);
})



class DateTimeInput extends HTMLElement {
    constructor () {
        super();
        this.innerHTML = '<input type="date" /><input type="time" />'
    }

    static get observedAttributes () {
        return [ 'dateonly' ]
    }

    attributeChangedCallback (name, from, to) {
        console.log(this, name, boolean(from), boolean(to));
        switch (name) {
        case 'dateonly':
            this.querySelector('[type="time"]').disabled = boolean(to)
            break;
        }
    }

    get dateonly () {
        return boolean(this.getAttribute('dateonly'));
    }

    set dateonly (bool) {
        this.setAttribute ('dateonly', bool);
    }

    get value () {

        let dt;
        let date = this.querySelector("[type='date']").value;
        if (boolean(this.getAttribute('dateonly'))) {
            dt = parseDate(date);
            dt.type = 'date';
        } else {
            let time = this.querySelector("[type='time']").value;
            dt = parseDate(date + 'T' + time)
            dt.type = 'date-time';
        }
        return dt;
    }

    set value (new_value) {
        let date, time;
        if (new_value instanceof Date) {
            date = new_value.format("~L~Y-~m-~d");
            time = new_value.format("~L~H:~M:~S");
        } else {
            [date, time] = new_value.split('T')
        }
        this.querySelector("[type='date']").value = date;
        this.querySelector("[type='time']").value = time;
    }

    addEventListener(type, proc) {
        if (type != 'input') throw "Only input supported";

        this.querySelector("[type='date']").addEventListener(type, proc);
        this.querySelector("[type='time']").addEventListener(type, proc);
    }
}

customElements.define('date-time-input', DateTimeInput)

class PopupElement extends HTMLElement {
    constructor () {
        super();

        /* TODO populate remaining */
        // this.id = 'popup' + this.dataset.uid
    }

    redraw () {
        console.log('IMPLEMENT ME');
    }

    connectedCallback() {
        let body = document.getElementById('popup-template').content.cloneNode(true).firstElementChild;

        let uid = this.dataset.uid
        // console.log(uid);

        body.getElementsByClassName('populate-with-uid')
            .forEach((e) => e.setAttribute('data-uid', uid));

        /* tabs */
        let tabgroup_id = gensym();
        for (let tab of body.querySelectorAll(".tabgroup .tab")) {
            let new_id = gensym();
            let input = tab.querySelector("input");
            input.id = new_id;
            input.name = tabgroup_id;
            tab.querySelector("label").setAttribute('for', new_id);
        }
        /* end tabs */

        /* nav bar */
        let nav = body.getElementsByClassName("popup-control")[0];
        bind_popup_control(nav);

        let btn = body.querySelector('.popup-control .close-tooltip')
        btn.addEventListener('click', () => {
            close_popup(this);
        });
        /* end nav bar */

        this.replaceChildren(body);
    }
}

window.addEventListener('load', function () {
    customElements.define('popup-element', PopupElement)
});

function wholeday_checkbox (box) {
    box.closest('.timeinput')
        .getElementsByTagName('date-time-input')
        .forEach(el => el.dateonly = box.checked);
}
