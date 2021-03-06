/*
  Properties are icalendar properties.

  p['name'] to get and set value (also updates any connected slots)

  p['_value_name'] for raw value
  p['_slot_name'] for connected slots, Vector of pairs, where the
                  car should be a reference to the slot, and the
                  cdr a procedure which takes a slot and a value
                  and binds the value to the slot.
 */
class VComponent {

    constructor(el, wide_event=false) {
        el.properties = this;
        this.html_element = el;

        /*
          List of field listeners, which are all notified whenever
          the listened for field is updated.
          - keys are field names
          - values MUST be a pair of
            + a javascript object to update
            + a prodecude taking that object, and the new value
         */
        this._slots = {}

        /* VCalParameter objects */
        this._values = {}

        /*
          All properties on this object which are part of the vcomponent.
          Ideally simply looping through all javascript fields would be nice,
          but we only want to export some.

          Popuplated by iCalendars built in types per default, and extended
         */
        this.ical_properties = new Set();

        let popup = popup_from_event(el);
        // let children = el.getElementsByTagName("properties")[0].children;

        /* actual component (not popup) */
        /*
          for (let e of el.querySelectorAll(".bind")) {
          }
        */

        /* bind_recur */

        /* primary display tab */

        let p;
        let lst = [...popup.querySelectorAll(".bind"),
                   ...el.querySelectorAll('.bind')];
        for (let e of lst) {
            // if (e.classList.contains('summary')) {
            //     console.log(e, e.closest('[data-bindby]'));
            // }
            if ((p = e.closest('[data-bindby]'))) {
                // console.log(p);
                // console.log(p.dataset.bindby);
                eval(p.dataset.bindby)(el, e);
            } else {
                // if (e.classList.contains('summary')) {
                //     /* TODO transfer data from backend to frontend in a better manner */
                //     console.log (this.get_callback_list(e.dataset.property));
                // }
                let f = (s, v) => {
                    console.log(s, v);
                    s.innerText = v.format(s.dataset && s.dataset.fmt);
                };
                this.get_callback_list(e.dataset.property).push([e, f]);
                // if (e.classList.contains('summary')) {
                //     console.log (this.get_callback_list(e.dataset.property));
                // }
                // console.log("registreing", e, e.dataset.property, this);
            }
        }

        /* checkbox for whole day */

        /* Popuplate default types, see types.js for property_names */
        for (let property of property_names) {
            this.ical_properties.add(property)
            // console.log("prop", property)

            this.create_property(property);
        }

        /* icalendar properties */
        for (let child of el.querySelector("vevent > properties").children) {
            /* child ≡ <dtstart><date-time>...</date-time></dtstart> */

            let field = child.tagName;
            // // let lst = get_property(el, field);
            // let lst = this.get(field);

            this.ical_properties.add(field)

            /* Bind vcomponent fields for this event */
            for (let s of el.querySelectorAll(`${field} > :not(parameters)`)) {
                /* s ≡ <date-time>...</date-time> */

                /* Binds value from XML-tree to javascript object
                   [parsedate]

                   TODO capture xcal type here, to enable us to output it to jcal later.
                */
                let parsedValue;
                let type = s.tagName.toLowerCase();
                switch (type) {
                case 'float':
                case 'integer':
                    parsedValue = Number(s.textContent);
                    break;

                case 'date-time':
                case 'date':
                    parsedValue = parseDate(s.textContent);
                    break;

                    /* TODO */
                case 'duration':
                    let start = s.getElementsByTagName('start');
                    let end = s.getElementsByTagName('end, duration');
                    if (end.tagName === 'period') {
                        parsePeriod(end.textContent);
                    }
                    break;
                    /* TODO */
                case 'period':
                    parsedValue = parsePeriod(s.textContent);
                    break;
                    /* TODO */
                case 'utc-offset':
                    break;

                case 'recur':
                    parsedValue = recur_xml_to_rrule(s);
                    break;

                case 'boolean':
                    switch (s.textContent) {
                    case 'true':  parsedValue = true; break;
                    case 'false': parsedValue = false; break;
                    default: throw "Value error"
                    }
                    break;


                case 'binary':
                    /* Binary is going to be BASE64 decoded, allowing us to ignore
                       it and handle it as a string for the time being */
                case 'cal-address':
                case 'text':
                case 'uri':
                    parsedValue = s.textContent;
                    // parsedValue.type = type;
                    break;

                default:
                    parsedValue = s.textContent;
                }


                // this['_value_rrule'] = new VCalParameter(type, parsedValue);
                // console.log("set", field, type, parsedValue);
                this._values[field] = new VCalParameter(type, parsedValue);
                if (! this._slots[field]) {
                    this._slots[field] = [];
                }
            }
        }

        /* set up graphical display changes */
        let container = el.closest(".event-container");
        if (container === null) {
            console.log("No enclosing event container for", el);
            return;
        }
        let start = parseDate(container.dataset.start);
        let end = parseDate(container.dataset.end);

        if (this.dtstart) {
            /* [parsedate] */
            // el.properties.dtstart = parseDate(el.properties.dtstart);
            this.get_callback_list('dtstart').push(
                [el.style, (s, v) => {
                    console.log(v);
                    s[wide_event?'left':'top'] = 100 * (to_local(v) - start)/(end - start) + "%";
                } ]);
        }


        if (this.dtend) {
            // el.properties.dtend = parseDate(el.properties.dtend);
            this.get_callback_list('dtend').push(
                // TODO right and bottom only works if used from the start. However,
                // events from the backend instead use top/left and width/height.
                // Normalize so all use the same, or find a way to convert between.
                [el.style,
                 (s, v) => s[wide_event?'right':'bottom'] = 100 * (1 - (to_local(v)-start)/(end-start)) + "%"]);
        }


        /* ---------- Calendar ------------------------------ */

        if (! el.dataset.calendar) {
            el.dataset.calendar = "Unknown";
        }

        let calprop = this.get_callback_list('calendar');
        this.create_property('calendar');
        this._values['calendar'] =
            new VCalParameter('INVALID', el.dataset.calendar);

        const rplcs = (s, v) => {
            let [_, calclass] = s.classList.find(/^CAL_/);
            s.classList.replace(calclass, "CAL_" + v);
        }

        calprop.push([popup, rplcs]);
        calprop.push([el, rplcs]);
        calprop.push([el, (s, v) => s.dataset.calendar = v]);



        /* ---------- /Calendar ------------------------------ */
    }


    /*
      Returns the _value_ slot of given field in event, creating it if needed .
      el - the event to work on
      field - name of the field
      default_value - default value when creating
      bind_to_ical - should this property be added to the icalendar subtree?
    */
    get_callback_list(field) {
        // let el = this.html_element;
        if (! this._slots[field]) {
            this._slots[field] = [];
        }

        // console.log("get", field);
        return this._slots[field];
    }

    to_jcal() {
        let properties = [];

        /* ??? */
        // for (let prop of event.properties.ical_properties) {
        for (let prop of this.ical_properties) {
            // console.log(prop);
            let v = this[prop];
            if (v !== undefined) {
                let sub = v.to_jcal();
                sub.unshift(prop);
                properties.push(sub);
            }
        }

        return ['vevent', properties, [/* alarms go here */]]
    }

    create_property(property_name) {
        Object.defineProperty(
            this, property_name,
            {
                /* TODO there is an assymetry here with .value needing to be called for
                   get:ed stuff, but set MUST be an unwrapped item.
                   Fix this.
                   */
                get: function() {
                    return this._values[property_name];
                },
                set: function (value) {
                    console.log("set", property_name, value);
                    /* Semi dirty hack to add properties which are missing.
                       Since we initialize without a type just guess depending
                       on the field name */
                    if (! this._values[property_name]) {
                        let type_arr
                            = valid_input_types[property_name.toUpperCase()]
                            || ['unknown'];
                        let type = type_arr[0];
                        /* Types which can take arrays are interesting */
                        if (type instanceof Array) {
                            type = type[0];
                        }
                        this._values[property_name]
                            = new VCalParameter(type, value)
                    } else {
                        this._values[property_name].value = value;
                    }
                    console.log(this._slots[property_name].length,
                                this._slots[property_name]);
                    /* TODO validate type */
                    /* See valid_input_types and all_types */
                    for (let [slot,updater] of this._slots[property_name]) {
                        // console.log(updater, slot);
                        updater(slot, value);
                    }
                },
            });
    }

}



/* "Body" of a vcomponent field.
   For example, given the JCal
   ["dtstamp", {}, "date-time", "2006-02-06T00:11:21Z"],
   this class would have
   VCalParameter {
       type = "date-time",
       properties = {},
       _value = new Date(2006,1,6,0,11,21)
    }
    And returns [{}, "date-time", "2006-02-06T00:11:21Z"]
    when serialized
   */
class VCalParameter {
    constructor (type, value, properties={}) {
        this.type = type;
        this._value = value;
        this.properties = properties;
    }

    get value() {
        return this._value;
    }

    set value(v) {
        this._value = v;
    }

    to_jcal() {
        let value;
        let v = this._value;
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
        return [this.properties, this.type, value];
    }
}
