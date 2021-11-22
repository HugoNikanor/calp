import { uid, ical_type, valid_input_types, JCal, JCalProperty } from './types'
import { parseDate } from './lib'

export {
    VEvent, xml_to_vcal,
    RecurrenceRule
}

/* Something which can be redrawn */
interface Redrawable extends HTMLElement {
    redraw: ((data: VEvent) => void)
}

class VEventValue {

    type: ical_type
    value: any
    parameters: Map<string, any>

    constructor(type: ical_type, value: any, parameters = new Map()) {
        this.type = type;
        this.value = value;
        this.parameters = parameters;
    }

    to_jcal(): [Map<string, any>, ical_type, any] {
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

/*
  Abstract representation of a calendar event (or similar).
All "live" calendar data in the frontend should live in an object of this type.
 */
class VEvent {

    /* Calendar properties */
    properties: Map<uid, VEventValue>

    /* Children (such as alarms for events) */
    components: VEvent[]

    /* HTMLElements which wants to be redrawn when this object changes.
       Elements can be registered with the @code{register} method.
     */
    registered: Redrawable[]

    _calendar: string | null = null;

    constructor(properties: Map<string, VEventValue> = new Map(), components: VEvent[] = []) {
        this.components = components;
        this.registered = [];
        /* Re-normalize all given keys to upper case. We could require
         * that beforehand, this is much more reliable, for only a
         * marginal performance hit.
         */
        this.properties = new Map;
        for (const [key, value] of properties) {
            this.properties.set(key.toUpperCase(), value);
        }
    }

    getProperty(key: string): any | undefined {
        key = key.toUpperCase()
        let e = this.properties.get(key);
        if (!e) return e;
        return e.value;
    }

    get boundProperties(): IterableIterator<string> {
        return this.properties.keys()
    }

    __setPropertyInternal(key: string, value: any, type?: ical_type) {
        key = key.toUpperCase();
        let e = this.properties.get(key);
        if (e) {
            if (type) { e.type = type; }
            e.value = value;
            return;
        }
        if (!type) {
            let type_ = valid_input_types.get(key)
            if (type_ === undefined) {
                type = 'unknown'
            } else if (type_ instanceof Array) {
                type = type_[0]
            } else {
                type = type_
            }
        }
        e = new VEventValue(type, value)
        this.properties.set(key, e);
    }

    setProperty(key: string, value: any, type?: ical_type) {
        this.__setPropertyInternal(key, value, type);
        for (let el of this.registered) {
            el.redraw(this);
        }
    }

    setProperties(pairs: [string, any, ical_type?][]) {
        for (let pair of pairs) {
            this.__setPropertyInternal(...pair);
        }
        for (let el of this.registered) {
            el.redraw(this);
        }
    }


    set calendar(calendar: string | null) {
        this._calendar = calendar;
        for (let el of this.registered) {
            el.redraw(this);
        }
    }

    get calendar(): string | null {
        return this._calendar;
    }

    register(htmlNode: Redrawable) {
        this.registered.push(htmlNode);
    }

    to_jcal(): JCal {
        let out_properties: JCalProperty[] = []
        console.log(this.properties);
        for (let [key, value] of this.properties) {
            let prop: JCalProperty = [
                key.toLowerCase(),
                ...value.to_jcal(),
            ]
            out_properties.push(prop);
        }
        return ['vevent', out_properties, [/* alarms go here*/]]
    }
}

function make_vevent_value(value_tag: Element) {
    /* TODO parameters */
    return new VEventValue(
        /* TODO error on invalid type? */
        value_tag.tagName as ical_type,
        make_vevent_value_(value_tag));
}


// 



type freqType = 'SECONDLY' | 'MINUTELY' | 'HOURLY' | 'DAILY' | 'WEEKLY' | 'MONTHLY' | 'YEARLY'
type weekday = 'MO' | 'TU' | 'WE' | 'TH' | 'FR' | 'SA' | 'SU'

class RecurrenceRule {
    freq?: freqType
    until?: Date
    count?: number
    interval?: number
    bysecond?: number[]
    byminute?: number[]
    byhour?: number[]
    byday?: (weekday | [number, weekday])[]
    bymonthday?: number[]
    byyearday?: number[]
    byweekno?: number[]
    bymonth?: number[]
    bysetpos?: number[]
    wkst?: weekday

    to_jcal() {
        let obj: any = {}
        if (this.freq) obj['freq'] = this.freq;
        if (this.until) obj['until'] = this.until.format(this.until.dateonly
            ? '~Y-~M~D'
            : '~Y-~M~DT~H:~M:~S');
        if (this.count) obj['count'] = this.count;
        if (this.interval) obj['interval'] = this.interval;
        if (this.bysecond) obj['bysecond'] = this.bysecond;
        if (this.byminute) obj['byminute'] = this.byminute;
        if (this.byhour) obj['byhour'] = this.byhour;
        if (this.bymonthday) obj['bymonthday'] = this.bymonthday;
        if (this.byyearday) obj['byyearday'] = this.byyearday;
        if (this.byweekno) obj['byweekno'] = this.byweekno;
        if (this.bymonth) obj['bymonth'] = this.bymonth;
        if (this.bysetpos) obj['bysetpos'] = this.bysetpos;

        if (this.byday) {
            let outarr: string[] = []
            for (let byday of this.byday) {
                if (byday instanceof Array) {
                    let [num, day] = byday;
                    outarr.push(`${num}${day}`)
                } else {
                    outarr.push(byday)
                }
            }
            obj['byday'] = outarr
        }

        if (this.wkst) obj['wkst'] = this.wkst;

        return obj;
    }
}

function xml_to_recurrence_rule(xml: Element): RecurrenceRule {
    let rr = new RecurrenceRule;

    if (xml.tagName.toLowerCase() !== 'recur') {
        throw new TypeError();
    }
    let by = new Map<string, any>([
        ['bysecond', []],
        ['byminute', []],
        ['byhour', []],
        ['bymonthday', []],
        ['byyearday', []],
        ['byweekno', []],
        ['bymonth', []],
        ['bysetpos', []],
        ['byday', []],
    ]);


    for (let child of xml.children) {
        /* see appendix a 3.3.10 RECUR of RFC 6321 */
        let t = child.textContent || '';
        let tn = child.tagName.toLowerCase()

        switch (tn) {
            case 'freq':
                rr.freq = t as freqType
                break;

            case 'until':
                rr.until = parseDate(t);
                break;

            case 'count':
            case 'interval':
                rr.count = Number(t)
                break;

            case 'bysecond':
            case 'byminute':
            case 'byhour':
            case 'bymonthday':
            case 'byyearday':
            case 'byweekno':
            case 'bymonth':
            case 'bysetpos':
                by.get(tn)!.push(Number(t));
                break;

            case 'byday':
                // xsd:integer? type-weekday
                let m = t.match(/([+-]?[0-9]*)([A-Z]{2})/)
                if (m == null) throw new TypeError()
                else if (m[1] === '') by.get('byday')!.push(m[2] as weekday)
                else by.get('byday')!.push([Number(m[1]), m[2] as weekday])
                break;

            case 'wkst':
                rr.wkst = t as weekday
                break;
        }
    }

    for (let [key, value] of by) {
        if (!value || value.length == 0) continue;
        (rr as any)[key] = value;
    }

    return rr;
}

// 


function make_vevent_value_(value_tag: Element) {
    /* RFC6321 3.6. */
    switch (value_tag.tagName) {
        case 'binary':
            /* Base64 to binary
               Seems to handle inline whitespace, which xCal standard reqires
               */
            return atob(value_tag.textContent || '')

        case 'boolean':
            switch (value_tag.textContent) {
                case 'true': return true;
                case 'false': return false;
                default:
                    console.warn(`Bad boolean ${value_tag.textContent}, defaulting with !!`)
                    return !!value_tag.textContent;
            }

        case 'time':
        case 'date':
        case 'date-time':
            return parseDate(value_tag.textContent || '');

        case 'duration':
            /* TODO duration parser here 'P1D' */
            return value_tag.textContent;

        case 'float':
        case 'integer':
            return Number(value_tag.textContent);

        case 'period':
            /* TODO has sub components, meaning that a string wont do */
            let start = value_tag.getElementsByTagName('start')[0]
            parseDate(start.textContent || '');
            let other;
            if ((other = value_tag.getElementsByTagName('end')[0])) {
                return parseDate(other.textContent || '')
            } else if ((other = value_tag.getElementsByTagName('duration')[0])) {
                /* TODO parse duration */
                return other.textContent
            } else {
                console.warn('Invalid end to period, defaulting to 1H');
                return new Date(3600);
            }

        case 'recur':
            return xml_to_recurrence_rule(value_tag);

        case 'utc-offset':
            /* TODO parse */
            return "";

        default:
            console.warn(`Unknown type '${value_tag.tagName}', defaulting to string`)
        case 'cal-address':
        case 'uri':
        case 'text':
            return value_tag.textContent;
    }
}

function xml_to_vcal(xml: Element): VEvent {
    /* xml MUST have a VEVENT (or equivalent) as its root */
    let properties = xml.getElementsByTagName('properties')[0];
    let components = xml.getElementsByTagName('components')[0];

    let property_map = new Map()
    if (properties) {
        property_loop:
        for (var i = 0; i < properties.childElementCount; i++) {
            let tag = properties.childNodes[i];
            if (!(tag instanceof Element)) continue;
            let parameters = {};
            let value: VEventValue | VEventValue[] = [];
            value_loop: for (var j = 0; j < tag.childElementCount; j++) {
                let child = tag.childNodes[j];
                if (!(child instanceof Element)) continue;
                if (child.tagName == 'parameters') {
                    parameters = /* TODO handle parameters */ {};
                    continue value_loop;
                } else switch (tag.tagName) {
                    /* These can contain multiple value tags, per
                       RFC6321 3.4.1.1. */
                    case 'categories':
                    case 'resources':
                    case 'freebusy':
                    case 'exdate':
                    case 'rdate':
                        (value as VEventValue[]).push(make_vevent_value(child));
                        break;
                    default:
                        value = make_vevent_value(child);
                }
            }
            property_map.set(tag.tagName, value);
        }
    }

    let component_list = []
    if (components) {
        for (let child of components.childNodes) {
            if (!(child instanceof Element)) continue;
            component_list.push(xml_to_vcal(child))
        }
    }

    return new VEvent(property_map, component_list)
}
