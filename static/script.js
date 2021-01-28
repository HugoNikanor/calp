'use strict';

/*
  calp specific stuff
*/

class EventCreator {

    /* dynamicly created event when dragging */
    constructor() {
        this.event = false;
        this.event_start = { x: NaN, y: NaN };
        this.down_on_event = false;
    }

    create_empty_event () {
        let event = document.getElementById("event-template")
            .firstChild.cloneNode(true);
        let popup = document.getElementById("popup-template")
            .firstChild.cloneNode(true);

        popup.getElementsByClassName("edit-form")[0].onsubmit = function () {
            create_event(event);
            return false; /* stop default */
        }

        let id = gensym ("__js_event");

        // TODO remove button?
        // $("button 2??").onclick = `remove_event(document.getElementById('${id}'))`

        let tabgroup_id = gensym();
        for (let tab of popup.querySelectorAll(".tabgroup .tab")) {
            let new_id = gensym();
            let input = tab.querySelector("input");
            input.id = new_id;
            input.name = tabgroup_id;
            tab.querySelector("label").setAttribute('for', new_id);
        }

        let nav = popup.getElementsByClassName("popup-control")[0];
        bind_popup_control(nav);

        // TODO download links

        event.id = id;
        popup.id = "popup" + id;

        return [popup, event];
    }

    create_event_down (intended_target) {
        let that = this;
        return function (e) {
            /* Only trigger event creation stuff on actuall events background,
               NOT on its children */
            that.down_on_event = false;
            if (e.target != intended_target) return;
            that.down_on_event = true;

            that.event_start.x = e.clientX;
            that.event_start.y = e.clientY;
        }
    }

    /*
      round_to: what start and end times should round to when dragging, in fractions
      of the width of the containing container.

      TODO limit this to only continue when on the intended event_container.

      (event → [0, 1)), 𝐑, bool → event → ()
     */
    create_event_move(pos_in, round_to=1, wide_element=false) {
        let that = this;
        return function (e) {
            if (e.buttons != 1 || ! that.down_on_event) return;

            /* Create event when we start moving the mouse. */
            if (! that.event) {
                /* Small deadzone so tiny click and drags aren't registered */
                if (Math.abs(that.event_start.x - e.clientX) < 10
                    && Math.abs(that.event_start.y - e.clientY) < 5)
                { return; }

                /* only allow start of dragging on background */
                if (e.target != this) return;

                /* only on left click */
                if (e.buttons != 1) return;

                let [popup, event] = that.create_empty_event();
                that.event = event;

                /* TODO better solution to add popup to DOM */
                document.getElementsByTagName("main")[0].append(popup);

                /* [0, 1) -- where are we in the container */
                /* Ronud to force steps of quarters */
                /* NOTE for in-day events a floor here work better, while for
                   all day events I want a round, but which has the tip over point
                   around 0.7 instead of 0.5.
                   It might also be an idea to subtract a tiny bit from the short events
                   mouse position, since I feel I always get to late starts.
                */
                let time = round_time(pos_in(this, e), round_to);

                event.dataset.time1 = time;
                event.dataset.time2 = time;

                /* ---------------------------------------- */

                this.appendChild(event);

                /* requires that event is child of an '.event-container'. */
                bind_properties(event, wide_element);

                /* requires that dtstart and dtend properties are initialized */

                /* ---------------------------------------- */

                /* Makes all current events transparent when dragging over them.
                   Without this weird stuff happens when moving over them

                   This includes ourselves.
                */
                for (let e of this.children) {
                    e.style.pointerEvents = "none";
                }

            }

            let time1 = Number(that.event.dataset.time1);
            let time2 = that.event.dataset.time2 =
                round_time(pos_in(that.event.parentElement, e),
                           round_to);

            /* ---------------------------------------- */

            let event_container = that.event.closest(".event-container");

            /* These two are in UTC */
            let container_start = parseDate(event_container.dataset.start);
            let container_end = parseDate(event_container.dataset.end);

            /* ---------------------------------------- */

            /* ms */
            let duration = container_end - container_start;

            let start_in_duration = duration * Math.min(time1,time2);
            let end_in_duration   = duration * Math.max(time1,time2);

            /* Notice that these are converted to UTC, since the intervals are given
               in utc, and I only really care about local time (which a specific local
               timezone doesn't give me)
            */
            /* TODO Should these inherit UTC from container_*? */
            let d1 = new Date(container_start.getTime() + start_in_duration)
            let d2 = new Date(container_start.getTime() + end_in_duration)

            that.event.properties.dtstart = d1;
            that.event.properties.dtend = d2;
        }
    }

    create_event_finisher (callback) {
        let that = this;
        return function create_event_up (e) {
            if (! that.event) return;

            /* Restore pointer events for all existing events.
               Allow pointer events on our new event
            */
            for (let e of that.event.parentElement.children) {
                e.style.pointerEvents = "";
            }

            place_in_edit_mode(that.event);

            let localevent = that.event;
            that.event = null;

            callback (localevent);

        }
    }
}

/* This incarnation of this function only adds the calendar switcher dropdown.
   All events are already editable by switching to that tab.

   TODO stop requiring a weird button press to change calendar.
*/
function place_in_edit_mode (event) {
    let popup = document.getElementById("popup" + event.id)
    let container = popup.getElementsByClassName('dropdown-goes-here')[0]
    let calendar_dropdown = document.getElementById('calendar-dropdown-template').firstChild.cloneNode(true);

    let [_, calclass] = popup.classList.find(/^CAL_/);
    label: {
	for (let [i, option] of calendar_dropdown.childNodes.entries()) {
	    if (option.value === calclass.substr(4)) {
		calendar_dropdown.selectedIndex = i;
		break label;
	    }
	}
	/* no match, try find default calendar */
	let t;
	if ((t = calendar_dropdown.querySelector("[selected]"))) {
	    event.properties.calendar = t.value;
	}
    }


    /* Instant change while user is stepping through would be
     * preferable. But I believe that <option> first gives us the
     * input once selected */
    calendar_dropdown.onchange = function () {
        event.properties.calendar = this.value;
    }
    container.appendChild(calendar_dropdown);

    let tab = popup.getElementsByClassName("tab")[1];
    let radio = tab.getElementsByTagName("input")[0];
    radio.click();
    tab.querySelector("input[name='summary']").focus();
}

window.onload = function () {
    // let start_time = document.querySelector("meta[name='start-time']").content;
    // let end_time = document.querySelector("meta[name='end-time']").content;

    const button_updater = new ButtonUpdater(
        document.getElementById("today-button"),
        (e, d) => e.href = d.format('~Y-~m-~d') + ".html"
    );

    const sch = new SmallcalCellHighlight(
        document.querySelector('.small-calendar'))

    const timebar = new Timebar(/*start_time, end_time*/);

    timebar.update(new Date);
    sch.update(new Date);
    window.setInterval(() => {
        let d = new Date;
        timebar.update(d);
        button_updater.update(d);
        sch.update(d);
    }, 1000 * 60);

    /* Is event creation active? */
    if (EDIT_MODE) {
        let eventCreator = new EventCreator;
        for (let c of document.getElementsByClassName("events")) {
            c.onmousedown = eventCreator.create_event_down(c);
            c.onmousemove = eventCreator.create_event_move(
                (c,e) => e.offsetY / c.clientHeight,
                /* every quarter, every hour */
                1/(24*4), false
            );
            c.onmouseup = eventCreator.create_event_finisher(
                function (event) {
                    let popupElement = document.getElementById("popup" + event.id);
                    open_popup(popupElement);

                    popupElement.querySelector("input[name='summary']").focus();

                });
        }

        for (let c of document.getElementsByClassName("longevents")) {
            c.onmousedown = eventCreator.create_event_down(c);
            c.onmousemove = eventCreator.create_event_move(
                (c,e) => e.offsetX / c.clientWidth,
                /* every day, NOTE should be changed to check
                   interval of longevents */
                1/7, true
            );
            c.onmouseup = eventCreator.create_event_finisher(
                function (event) {
                    let popupElement = document.getElementById("popup" + event.id);
                    open_popup(popupElement);

					popupElement.querySelector("input[name='summary']").focus();

                });
        }
    }

    for (let nav of document.getElementsByClassName("popup-control")) {
        bind_popup_control(nav);
    }

    for (let el of document.getElementsByClassName("event")) {
        /* Popup script replaces need for anchors to events.
           On mobile they also have the problem that they make
           the whole page scroll there.
        */
        el.parentElement.removeAttribute("href");

        /* TODO this doesn't yet apply to newly created events */
        let popup = document.getElementById("popup" + el.id);
        popup.getElementsByClassName("edit-form")[0].onsubmit = function () {
            create_event(el);
            return false; /* stop default */
        }

        /* Bind all vcomponent properties into javascript. */
        if (el.closest(".longevents")) {
            bind_properties(el, true);
        } else {
            bind_properties(el, false);
        }

    }

    document.onkeydown = function (evt) {
	evt = evt || window.event;
        if (! evt.key) return;
	if (evt.key.startsWith("Esc")) {
	    close_all_popups();
	}
    }


    /* Replace backend-driven [today] link with frontend, with one that
       gets correctly set in the frontend. Similarly, update the go to
       specific date button into a link which updates wheneven the date
       form updates.
    */

    let gotodatebtn = document.querySelector("#jump-to .btn");
    let target_href = (new Date).format("~Y-~m-~d") + ".html";
    let golink = makeElement('a', {
        className: 'btn',
        href: target_href,
        innerHTML: gotodatebtn.innerHTML,
    });
    document.getElementById("today-button").href = target_href;
    gotodatebtn.replaceWith(golink);

    document.querySelector("#jump-to input[name='date']").onchange = function () {
        let date = this.valueAsDate.format("~Y-~m-~d");
        console.log(date);
        golink.href = date + ".html";
    }

    /* ---------------------------------------- */

    /* needs to be called AFTER bind_properties, but BEFORE init_input_list
       After bind_properties since that initializes categories to a possible field
       Before init_input_list since we need this listener to be propagated to clones.
       [CATEGORIES_BIND]
    */
    for (let lst of document.querySelectorAll(".input-list[data-property='categories']")) {
        let f = function () {
            console.log(lst, lst.closest('.popup-container'));
            let event = event_from_popup(lst.closest('.popup-container'))
            event.properties.categories = lst.get_value();
        };

        for (let inp of lst.querySelectorAll('input')) {
            inp.addEventListener('input', f);
        }
    }

    // init_arbitary_kv();

    init_date_time();
    init_input_list();

}


/*
  Returns the _value_ slot of given field in event, creating it if needed .
  el - the event to work on
  field - name of the field
  default_value - default value when creating
  bind_to_ical - should this property be added to the icalendar subtree?
*/
function get_property(el, field, default_value) {
    if (! el.properties) {
        /* TODO only have construction once */
        el.properties = {};
        el.properties.ical_properties = new Set()
    }

    if (! el.properties["_slot_" + field]) {
        el.properties["_slot_" + field] = [];
        el.properties["_value_" + field] = default_value;

        Object.defineProperty(
            el.properties, field,
            {
                get: function () {
                    return this["_value_" + field];
                },
                set: function (value) {
                    this["_value_" + field] = value;
                    for (let [slot,updater] of el.properties["_slot_" + field]) {
                        updater(slot, value);
                    }
                }
            });
    }

    return el.properties["_slot_" + field];
}



/*
  Properties are icalendar properties.

  p['name'] to get and set value (also updates any connected slots)

  p['_value_name'] for raw value
  p['_slot_name'] for connected slots, Vector of pairs, where the
                  car should be a reference to the slot, and the
                  cdr a procedure which takes a slot and a value
                  and binds the value to the slot.
 */
function bind_properties (el, wide_event=false) {

    el.properties = {}
    el.properties.ical_properties = new Set()
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
        if ((p = e.closest('[data-bindby]'))) {
            // console.log(p.dataset.bindby);
            eval(p.dataset.bindby)(el, e);
        } else {
            let f = ((s, v) => s.innerHTML = v.format(s.dataset && s.dataset.fmt));
            get_property(el, e.dataset.property).push([e, f]);
        }
    }

    // for (let e of popup.querySelectorAll(".summary-tab .bind")) {
    //     /* bind_view
    //     let f = (s, v) => s.innerHTML = v.format(s.dataset && s.dataset.fmt);
    //     get_property(el, e.dataset.property).push([e, f]);
    //     */
    // }

    /* edit tab */
    // for (let e of popup.querySelectorAll(".edit-tab .bind")) {
    //     /* bind-edit
    //     let p = get_property(el, e.dataset.property);
    //     e.addEventListener('input', function () {
    //         el.properties[e.dataset.property] = this.value;
    //     });
    //     let f;
    //     switch (e.tagName) {
    //     case 'input':
    //         switch (e.type) {
    //         case 'time': f = (s, v) => s.value = v.format("~H:~M"); break;
    //         case 'date': f = (s, v) => s.value = v.format("~Y-~m-~d"); break;
    //         // TODO remaining types cases
    //         default: f = (s, v) => s.value = v;
    //         }
    //         p.push([e, f])
    //         break;
    //     case 'textarea':
    //         f = (s, v) => s.innerHTML = v;
    //         p.push([e, f])
    //         break;
    //     default:
    //         alert("How did you get here??? " + e.tagName)
    //         break;
    //     }
    //     */
    // }

    /* checkbox for whole day */

    for (let field of ['dtstart', 'dtend']) {

        get_property(el, `--${field}-time`).push(
            [el, (el, v) => { let date = el.properties[field];
                             if (v == '') return;
                             let [h,m,s] = v.split(':')
                             date.setHours(Number(h));
                             date.setMinutes(Number(m));
                             date.setSeconds(0);
                             el.properties[field] = date; }])
        get_property(el, `--${field}-date`).push(
            [el, (el, v) => { let date = el.properties[field];
                             if (v == '') return;
                             let [y,m,d] = v.split('-')
                             date.setYear(Number(y)/* - 1900*/);
                             date.setMonth(Number(m) - 1);
                             date.setDate(d);
                             el.properties[field] = date; }])


        /* Manual fetch of the fields instead of the general method,
           to avoid an infinite loop of dtstart setting --dtstart-time,
           and vice versa.
           NOTE if many more fields require special treatment then a
           general solution is required.
        */
        get_property(el, field).push(
            [el, (el, v) => { popup
                            .querySelector(`.edit-tab input[name='${field}-time']`)
                            .value = v.format("~H:~M");
                            popup
                            .querySelector(`.edit-tab input[name='${field}-date']`)
                            .value = v.format("~Y-~m-~d");
                            }]);
    }

    for (let property of property_names) {
        el.properties.ical_properties.add(property)
    }

    /* icalendar properties */
    for (let child of el.querySelector("vevent > properties").children) {
        /* child ≡ <dtstart><date-time>...</date-time></dtstart> */

        let field = child.tagName;
        let lst = get_property(el, field);

        el.properties.ical_properties.add(field)

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
                parsedValue = new Number(s.innerHTML);
                parsedValue.type = type;
                break;

            case 'date-time':
            case 'date':
                parsedValue = parseDate(s.innerHTML);
                break;

                /* TODO */
            case 'duration':
                let start = s.getElementsByTagName('start');
                let end = s.getElementsByTagName('end, duration');
                if (end.tagName === 'period') {
                    parsePeriod(end.innerHTML);
                }
                break;
                /* TODO */
            case 'period':
                parsePeriod(s.innerHTML);
                break;
                /* TODO */
            case 'utc-offset':
                break;

            case 'recur':
                parsedValue = recur_xml_to_rrule(s);
                break;

            case 'boolean':
                switch (s.innerHTML) {
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
                /* TODO Attributes on strings doesn't work 
                   They do however work on String:s
                */
                parsedValue = s.innerHTML;
                // parsedValue.type = type;
                break;

            default:
                parsedValue.type = 'unknown';
                parsedValue = s.innerHTML;
            }
            el.properties['_value_rrule'] = parsedValue;
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

    if (el.properties.dtstart) {
        /* [parsedate] */
        // el.properties.dtstart = parseDate(el.properties.dtstart);
        get_property(el, 'dtstart').push(
            [el.style, (s, v) =>
             s[wide_event?'left':'top'] = 100 * (to_local(v) - start)/(end - start) + "%"]);
    }


    if (el.properties.dtend) {
        // el.properties.dtend = parseDate(el.properties.dtend);
        get_property(el, 'dtend').push(
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

    let calprop = get_property(el, 'calendar', el.dataset.calendar);

    const rplcs = (s, v) => {
        let [_, calclass] = s.classList.find(/^CAL_/);
        s.classList.replace(calclass, "CAL_" + v);
    }

    calprop.push([popup, rplcs]);
    calprop.push([el, rplcs]);
    calprop.push([el, (s, v) => s.dataset.calendar = v]);



    /* ---------- Calendar ------------------------------ */
}
