"use strict";

class VEventValue {
    constructor (type, value, parameters = {}) {
        this.type = type;
        this.value = value;
        this.parameters = parameters;
    }

    to_jcal () {
        let value;
        let v = this.value;
        switch (this.type) {
        case 'binary':
            /* TOOD */
            break;
        case 'date-time':
            value = v.format("~Y-~m-~dT~H:~M:~S");
            // TODO TZ
            break;
        case 'date':
            value = v.format("~Y-~m-~d");
            break;
        case 'duration':
            /* TODO */
            break;
        case 'period':
            /* TODO */
            break;
        case 'utc-offset':
            /* TODO */
            break;
        case 'recur':
            value = v.asJcal();
            break;

        case 'float':
        case 'integer':
        case 'text':
        case 'uri':
        case 'cal-address':
        case 'boolean':
            value = v;
        }
        return [this.parameters, this.type, value];
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
            console.log(el);
            el.redraw(this);
        }
    }

    register (htmlNode) {
        this.registered.push(htmlNode);
    }

    to_jcal () {
        let out_properties = []
        for (let [key, value] of Object.entries(this.properties)) {
            let sub = value.to_jcal();
            sub.unshift(key)
            out_properties.push(sub);
        }
        return ['vevent', out_properties, [/* alarms go here*/]]
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

    case 'utc-offset':
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
