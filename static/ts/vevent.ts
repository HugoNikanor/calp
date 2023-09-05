import { ical_type, valid_input_types, JCal, JCalProperty, ChangeLogEntry } from './types'
import { parseDate } from './lib'

export {
    VEvent, xml_to_vcal,
    RecurrenceRule,
    isRedrawable,
}

/** Something which can be redrawn */
interface Redrawable extends HTMLElement {
    redraw(data: VEvent): void
}

/** Checks if the given element is an instance of Redrawable. */
function isRedrawable(x: HTMLElement): x is Redrawable {
    return 'redraw' in x
}


class VEventValue {

    type: ical_type

    /* value should NEVER be a list, since multi-valued properties should
       be split into multiple VEventValue objects! */
    value: any
    parameters: Map<string, any>

    constructor(type: ical_type, value: any, parameters = new Map) {
        this.type = type;
        this.value = value;
        this.parameters = parameters;
    }

    /**
     * The return value is *almost* a `JCalProperty`, just without
     * the field name.
    */
    to_jcal(): [Record<string, any>, ical_type, any] {
        let value;
        let v = this.value;
        switch (this.type) {
            case 'binary':
                /* TODO */
                value = 'BINARY DATA GOES HERE';
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
                value = 'DURATION GOES HERE';
                break;
            case 'period':
                /* TODO */
                value = 'PERIOD GOES HERE';
                break;
            case 'utc-offset':
                /* TODO */
                value = 'UTC-OFFSET GOES HERE';
                break;
            case 'recur':
                value = v.to_jcal();
                break;

            case 'float':
            case 'integer':
            case 'text':
            case 'uri':
            case 'cal-address':
            case 'boolean':
                value = v;
        }

        return [this.parameters, this.type, value]
    }
}

/* TODO maybe ... */
class VEventDuration extends VEventValue {
}

type list_values
    = 'categories' | 'resources' | 'freebusy' | 'exdate' | 'rdate'
    | 'CATEGORIES' | 'RESOURCES' | 'FREEBUSY' | 'EXDATE' | 'RDATE';

/*
  Abstract representation of a calendar event (or similar).
All "live" calendar data in the frontend should live in an object of this type.
 */
/**
 * Component for a single instance of a calendar event. Almost all data
 * access should go through `getProperty` and `setProperty`,
 * with the exception of the current calendar (which is accessed directly
 * through `calendar`). Almost all changes through these interfaces
 * are logged, and can be viewed through `changelog`.
 */
class VEvent {

    /* Calendar properties */
    private properties: Map<string, VEventValue | VEventValue[]>

    /* Children (such as alarms for events) */
    components: VEvent[]

    /* HTMLElements which wants to be redrawn when this object changes.
       Elements can be registered with the @code{register} method.
     */
    registered: Redrawable[]

    #calendar: string | null = null;

    /**
     * Every write through getProperty gets logged here, and can be
     * consumed. Hopefully this will one day turn into an undo system.
     * TODO ref ChangeLogEntry.
     */
    #changelog: ChangeLogEntry[] = []

    /* Iterator instead of direct return to ensure the receiver doesn't
       modify the array */
    /** Public (read only) interface to changelog. */
    get changelog(): IterableIterator<[number, ChangeLogEntry]> {
        return this.#changelog.entries();
    }

    addlog(entry: ChangeLogEntry) {
        let len = this.#changelog.length
        let last = this.#changelog[len - 1]

        // console.log('entry = ', entry, ', last = ', last);

        if (!last) {
            // console.log('Adding new entry', entry, this.getProperty('uid'));
            this.#changelog.push(entry);
            return;
        }

        if (entry.type === last.type
            && entry.name === last.name
            && entry.from === last.to) {
            this.#changelog.pop();
            entry.from = last.from
            // console.log('Changing old entry', entry, this.getProperty('uid'));
            this.#changelog.push(entry)
        } else {
            this.#changelog.push(entry)
        }
    }

    constructor(
        properties: Map<string, VEventValue | VEventValue[]> = new Map(),
        components: VEvent[] = []
    ) {
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

    getProperty(key: list_values): any[] | undefined;
    getProperty(key: string): any | undefined;

    // getProperty(key: 'categories'): string[] | undefined

    /**
     * Returns the value of the given property if set, or undefined otherwise.
     *
     * For the keys
     *
     * - `'CATEGORIES'`,
     * - `'RESOURCES'`,
     * - `'FREEBUSY'`,
     * - `'EXDATE'`, and
     * - `'RDATE'`
     *
     * instead returns a list list of values.
     */
    getProperty(key: string): any | any[] | undefined {
        key = key.toUpperCase()
        let e = this.properties.get(key);
        if (!e) return e;
        if (Array.isArray(e)) {
            return e.map(ee => ee.value)
        }
        return e.value;
    }

    /** Returns an iterator of all our properties. */
    get boundProperties(): IterableIterator<string> {
        return this.properties.keys()
    }

    private setPropertyInternal(key: string, value: any, type?: ical_type) {
        function resolve_type(key: string, type?: ical_type): ical_type {
            if (type) {
                return type;
            } else {
                let type_options = valid_input_types.get(key)
                if (type_options === undefined) {
                    return 'unknown'
                } else if (type_options.length == 0) {
                    return 'unknown'
                } else {
                    if (Array.isArray(type_options[0])) {
                        return type_options[0][0]
                    } else {
                        return type_options[0]
                    }
                }
            }
        }

        key = key.toUpperCase();

        /*
          To is mostly for the user. From is to allow an undo button
         */
        let entry: ChangeLogEntry = {
            type: 'property',
            name: key,
            from: this.getProperty(key), // TODO what happens if getProperty returns a weird type
            to: '' + value,
        }
        // console.log('Logging ', entry);
        this.addlog(entry);


        if (Array.isArray(value)) {
            this.properties.set(key,
                value.map(el => new VEventValue(resolve_type(key, type), el)))
            return;
        }
        let current = this.properties.get(key);
        if (current) {
            if (Array.isArray(current)) {
                /* TODO something here? */
            } else {
                if (type) { current.type = type; }
                current.value = value;
                return;
            }
        }
        type = resolve_type(key, type);
        let new_value = new VEventValue(type, value)
        this.properties.set(key, new_value);
    }

    setProperty(key: list_values, value: any[], type?: ical_type): void;
    setProperty(key: string, value: any, type?: ical_type): void;

    /**
     * Sets the given property to the given value. If type is given it's
     * stored alongside the value, possibly updating what is already
     * there. Do however note that no validation between the given type and
     * the type of the value is done.
     *
     * `value` may also be a list, but should only be so for the keys
     * mentioned in `getProperty`.
     *
     * After the value is set, `redraw` is called on all registered
     * objects, notifying them of the change.
     */
    setProperty(key: string, value: any, type?: ical_type) {
        this.setPropertyInternal(key, value, type);

        for (let el of this.registered) {
            el.redraw(this);
        }
    }

    /**
     * Equivalent to running `setProperty` for each element in the input
     * list, but only calls `redraw` once at the end.
     */
    setProperties(pairs: [string, any, ical_type?][]) {
        for (let pair of pairs) {
            this.setPropertyInternal(...pair);
        }
        for (let el of this.registered) {
            el.redraw(this);
        }
    }

    /** The name of the calendar which this event belongs to. */
    set calendar(calendar: string | null) {
        this.addlog({
            type: 'calendar',
            name: '',
            from: this.#calendar,
            to: calendar,
        });
        this.#calendar = calendar;
        for (let el of this.registered) {
            el.redraw(this);
        }
    }

    get calendar(): string | null {
        return this.#calendar;
    }

    /**
     * Register something redrawable, which will be notified whenever this
     * VEvents data is updated.
     */
    register(htmlNode: Redrawable) {
        this.registered.push(htmlNode);
    }

    /** Stop recieving redraw events on the given component. */
    unregister(htmlNode: Redrawable) {
        this.registered = this.registered.filter(node => node !== htmlNode)
    }

    /** Converts the object to JCal data. */
    to_jcal(): JCal {
        let out_properties: JCalProperty[] = []
        console.log(this.properties);
        for (let [key, value] of this.properties) {
            console.log("key = ", key, ", value = ", value);
            if (Array.isArray(value)) {
                if (value.length == 0) continue;
                let mostly = value.map(v => v.to_jcal())
                let values = mostly.map(x => x[2])
                console.log("mostly", mostly)
                out_properties.push([
                    key.toLowerCase(),
                    mostly[0][0],
                    mostly[0][1],
                    ...values
                ])
            } else {
                let prop: JCalProperty = [
                    key.toLowerCase(),
                    ...value.to_jcal(),
                ]
                out_properties.push(prop);
            }
        }

        return ['vevent', out_properties, [/* alarms go here*/]]
    }
}

function make_vevent_value(value_tag: Element): VEventValue {
    /* TODO parameters */
    return new VEventValue(
        /* TODO error on invalid type? */
        value_tag.tagName.toLowerCase() as ical_type,
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

    /** Converts ourselves to JCal data. */
    to_jcal(): Record<string, any> {
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

/** Parse a XCAL recurrence rule into a RecurrenceRule object. */
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
                rr.count = Number(t)
                break;

            case 'interval':
                rr.interval = Number(t)
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


function make_vevent_value_(value_tag: Element): string | boolean | Date | number | RecurrenceRule {
    /* RFC6321 3.6. */
    switch (value_tag.tagName.toLowerCase()) {
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
            return value_tag.textContent || '';

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
                return other.textContent || ''
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
            return value_tag.textContent || '';
    }
}

/** Parse a complete XCAL object into a JS VEvent object. */
function xml_to_vcal(xml: Element): VEvent {
    /* xml MUST have a VEVENT (or equivalent) as its root */
    let properties = xml.getElementsByTagName('properties')[0];
    let components = xml.getElementsByTagName('components')[0];

    let property_map: Map<string, VEventValue | VEventValue[]> = new Map;
    if (properties) {
        /* property_loop: */
        for (var i = 0; i < properties.childElementCount; i++) {
            let tag = properties.childNodes[i];
            if (!(tag instanceof Element)) continue;
            let parameters = {};
            let value: VEventValue | VEventValue[] = [];
            value_loop:
            for (var j = 0; j < tag.childElementCount; j++) {
                let child = tag.childNodes[j];
                if (!(child instanceof Element)) continue;
                if (child.tagName.toLowerCase() == 'parameters') {
                    parameters = /* TODO handle parameters */ {};
                    continue value_loop;
                } else switch (tag.tagName.toLowerCase()) {
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
            property_map.set(tag.tagName.toLowerCase(), value);
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
