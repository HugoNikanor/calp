"use strict";

class VEventValue {
    constructor (type, value, parameters = {}) {
        this.type = type;
        this.value = value;
        this.parameters = parameters;
    }
}

/* maybe ... */
class VEventDuration extends VEventValue {
}

class VEvent {
    constructor (properties = {}, components = []) {
        this.properties = properties;
        this.components = components;
        this.registered = [];
    }

    getProperty (key) {
        let e = this.properties[key];
        if (! e) return e;
        return e.value;
    }

    setProperty (key, value) {
        let e = this.properties[key];
        if (! e) {
            let type = (valid_input_types[key.toUpperCase()] || ['unknown'])[0]
            if (typeof type === typeof []) type = type[0];
            e = this.properties[key] = new VEventValue(type, value);
        } else {
            e.value = value;
        }
        for (let el of this.registered) {
            /* TODO update correct fields, allow component to redraw themselves */
            el.redraw(this);
        }
    }

    register (htmlNode) {
        this.registered.push(htmlNode);
    }
}

function make_vevent_value (value_tag) {
    /* TODO parameters */
    return new VEventValue (value_tag.tagName, make_vevent_value_ (value_tag));
}

function make_vevent_value_ (value_tag) {
    /* RFC6321 3.6. */
    switch (value_tag.tagName) {
    case 'binary':
        /* Base64 to binary
           Seems to handle inline whitespace, which xCal standard reqires
           */
        return atob(value_tag.innerHTML)
        break;

    case 'boolean':
        switch (value_tag.innerHTML) {
        case 'true':  return true;
        case 'false': return false;
        default:
            console.warn(`Bad boolean ${value_tag.innerHTML}, defaulting with !!`)
            return !! value_tag.innerHTML;
        }
        break;

    case 'time':
    case 'date':
    case 'date-time':
        return parseDate(value_tag.innerHTML);
        break;

    case 'duration':
        /* TODO duration parser here 'P1D' */
        return value_tag.innerHTML;
        break;

    case 'float':
    case 'integer':
        return +value_tag.innerHTML;
        break;

    case 'period':
        /* TODO has sub components, meaning that a string wont do */
        let start = value_tag.getElementsByTagName('start')[0]
        parseDate(start.innerHTML);
        let other;
        if ((other = value_tag.getElementsByTagName('end')[0])) {
            return parseDate(other.innerHTML)
        } else if ((other = value_tag.getElementsByTagName('duration')[0])) {
            /* TODO parse duration */
            return other.innerHTML
        } else {
            console.warn('Invalid end to period, defaulting to 1H');
            return new Date(3600);
        }

    case 'recur':
        /* TODO parse */
        return "";

    case 'uc-offset':
        /* TODO parse */
        return "";

    default:
        console.warn(`Unknown type '${value_tag.tagName}', defaulting to string`)
    case 'cal-address':
    case 'uri':
    case 'text':
        return value_tag.innerHTML;
    }
}


/* xml dom object -> class VEvent */
function xml_to_vcal (xml) {
    /* xml MUST have a VEVENT (or equivalent) as its root */
    let properties = xml.getElementsByTagName('properties')[0];
    let components = xml.getElementsByTagName('components')[0];

    let property_map = {}
    if (properties) {
        for (var i = 0; i < properties.childElementCount; i++) {
            let tag = properties.childNodes[i];
            let parameters = {};
            let value = [];
            for (var j = 0; j < tag.childElementCount; j++) {
                let child = tag.childNodes[j];
                switch (tag.tagName) {
                case 'parameters':
                    parameters = /* handle parameters */ {};
                    break;

                    /* These can contain multiple value tags, per
                       RFC6321 3.4.1.1. */
                case 'categories':
                case 'resources':
                case 'freebusy':
                case 'exdate':
                case 'rdate':
                    value.push(make_vevent_value(child));
                    break;
                default:
                    value = make_vevent_value(child);
                }
            }
            property_map[tag.tagName] = value;
        }
    }

    let component_list = []
    if (components) {
        for (let child of components.childNodes) {
            component_list.push(xml_to_vcal(child))
        }
    }

    return new VEvent(property_map, component_list)
}

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
        this.redraw(vcal_objects[this.dataset.uid]);

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

class ComponentBlock extends ComponentVEvent {
    constructor () {
        super();
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

function wholeday_checkbox (box) {
    box.closest('.timeinput')
        .getElementsByTagName('date-time-input')
        .forEach(el => el.dateonly = box.checked);
}
