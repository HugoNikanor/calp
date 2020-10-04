'use strict';

/*
  calp specific stuff
*/

let parser = new DOMParser();

/* start and end time for calendar page */
let start_time = new Date();
let end_time = new Date();

function bind_popup_control (nav) {
    nav.onmousedown = function (e) {
        /* Ignore mousedown on children */
        if (e.target != nav) return;
        nav.style.cursor = "grabbing";
        nav.dataset.grabbed = "true";
        nav.dataset.grabPoint = e.clientX + ";" + e.clientY;
        let popup = nav.closest(".popup-container");
        nav.dataset.startPoint = popup.offsetLeft + ";" + popup.offsetTop;
    }
    window.addEventListener('mousemove', function (e) {
        if (nav.dataset.grabbed) {
            let [x, y] = nav.dataset.grabPoint.split(";").map(Number);
            let [startX, startY] = nav.dataset.startPoint.split(";").map(Number);
            let popup = nav.closest(".popup-container");

            popup.style.left = startX + (e.clientX - x) + "px";
            popup.style.top = startY + (e.clientY - y) + "px";
        }
    });
    window.addEventListener('mouseup', function () {
        nav.dataset.grabbed = "";
        nav.style.cursor = "";
    });
}

/*
 * Finds the first element of the DOMTokenList whichs value matches
 * the supplied regexp. Returns a pair of the index and the value.
 */
DOMTokenList.prototype.find = function (regexp) {
    let entries = this.entries();
    let entry;
    while (! (entry = entries.next()).done) {
        if (entry.value[1].match(regexp)) {
            return entry.value;
        }
    }
}

class EventCreator {

    /* dynamicly created event when dragging */
    constructor() {
        this.event = false;
        this.event_start = { x: NaN, y: NaN };
        this.down_on_event = false;

    }

    create_empty_event () {
        let event = document.getElementById("event-template").firstChild.cloneNode(true);
        let popup = document.getElementById("popup-template").firstChild.cloneNode(true);

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
                place_in_edit_mode(event);

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

            let localevent = that.event;
            that.event = null;

            callback (localevent);

        }
    }
}

async function remove_event (element) {
    let uid = element.querySelector("icalendar uid text").innerHTML;

    let data = new URLSearchParams();
    data.append('uid', uid);

    let response = await fetch ( '/remove', {
        method: 'POST',
        body: data
    });

    console.log(response);
    toggle_popup("popup" + element.id);

    if (response.status < 200 || response.status >= 300) {
        let body = await response.text();
        alert(`HTTP error ${response.status}\n${body}`)
    } else {
        element.remove();
    }
}

var bar_object = false
var current_cell = false

function update_current_time_bar () {
    var now = new Date()
    if (! (start_time <= now.getTime() && now.getTime() < end_time))
        return;

    var event_area = document.getElementById(now.format("~Y-~m-~d"))

    if (event_area) {
        if (bar_object) {
            bar_object.parentNode.removeChild(bar_object)
        } else {
            bar_object = makeElement ('div', {
                id: 'bar',
                className: 'eventlike current-time',
            });
        }

        bar_object.style.top = date_to_percent(now) + "%";
        event_area.append(bar_object)
    }

    /* */

    if (current_cell) {
        current_cell.style.border = "";
    }
    current_cell = document.querySelector(
        ".small-calendar time[datetime='" + now.format("~Y-~m-~d") + "']");
    current_cell.style.border = "1px solid black";

    /* Update [today] button */

    document.getElementById("today-button").href
        = (new Date).format("~Y-~m-~d") + ".html";
}

function close_all_popups () {
    for (let popup of document.querySelectorAll(".popup-container.visible")) {
        close_popup(popup);
    }
}

async function create_event (event) {

    let xml = event.getElementsByTagName("icalendar")[0].outerHTML
    let calendar = event.properties.calendar;

    console.log(calendar, xml);

    let data = new URLSearchParams();
    data.append("cal", calendar);
    data.append("data", xml);

    let response = await fetch ( '/insert', {
        method: 'POST',
        body: data
    });

    console.log(response);
    if (response.status < 200 || response.status >= 300) {
        let body = await response.text();
        alert(`HTTP error ${response.status}\n${body}`)
        return;
    }

    let body = await response.text();

    /* servere is assumed to return an XML document on the form
       <properties>
       **xcal property** ...
       </properties>
       parse that, and update our own vevent with the data.
    */

    let properties = parser
        .parseFromString(body, 'text/xml')
        .children[0];

    let child;
    while ((child = properties.firstChild)) {
        let target = event.querySelector(
            "vevent properties " + child.tagName);
        if (target) {
            target.replaceWith(child);
        } else {
            event.querySelector("vevent properties")
                .appendChild(child);
        }
    }

    event.classList.remove("generated");
    toggle_popup("popup" + event.id);
}

function place_in_edit_mode (event) {
    let popup = document.getElementById("popup" + event.id)
    function replace_with_time_input(fieldname, event) {
        let field = popup.getElementsByClassName(fieldname)[0];

        let dt = new Date(field.dateTime);

        let dateinput = makeElement ('input', {
            type: 'date',
            required: true,
            value: dt.format("~Y-~m-~d"),

            onchange: function (e) {
                /* Only update datetime when the input is filled out */
                if (! this.value) return;
                let [year, month, day] = this.value.split("-").map(Number);
                /* retain the hour and second information */
                let d = copyDate(event.properties[fieldname]);
                d.setYear(year);
                d.setMonth(month - 1);
                d.setDate(day);
                event.properties[fieldname] = d;
            }
        });

        let timeinput = makeElement ('input', {
            type: "time",
            required: true,
	    value: dt.format("~H:~M"),

            onchange: function (e) {
                /* Only update datetime when the input is filled out */
                if (! this.value) return;
                let [hour, minute] = this.value.split(":").map(Number);
                /* retain the year, month, and day information */
                let d = copyDate(event.properties[fieldname]);
                d.setHours(hour);
                d.setMinutes(minute);
                event.properties[fieldname] = d;
            }
        });
        let slot = get_property(event, fieldname);
        let idx = slot.findIndex(e => e[0] === field);
        slot.splice(idx, 1, [timeinput, (s, v) => s.value = v.format("~H:~M")])
        slot.splice(idx, 0, [dateinput, (s, v) => s.value = v.format("~Y-~m-~d")])

        field.innerHTML = '';
        field.appendChild(dateinput);
        field.appendChild(timeinput);
        // field.replaceWith(timeinput);

    }

    /* TODO ensure dtstart < dtend */
    replace_with_time_input("dtstart", event);
    replace_with_time_input("dtend", event);

    /* ---------------------------------------- */

    let summary = popup.getElementsByClassName("summary")[0];
    let input = makeElement('input', {
        name: "summary",
        value: summary.innerText,
        placeholder: "Sammanfattning",
        required: true,
    });

    input.oninput = function () {
        event.properties["summary"] = this.value;
    }

    let slot = get_property(event, "summary");
    let idx = slot.findIndex(e => e[0] === summary);
    slot.splice(idx, 1, [input, (s, v) => s.value = v])

    summary.replaceWith(input);

    /* ---------------------------------------- */

    /* TODO add elements if the arent't already there
     * Almost all should be direct children of '.event-body' (or
         * '.eventtext'?).
     * Biggest problem is generated fields relative order.
     */
    {
        let descs = popup.getElementsByClassName("description");
        let description;
        if (descs.length === 1) {
            description = descs[0];
        } else {
            let fields = popup.getElementsByClassName("fields")[0]
            description = makeElement('span', {
                class: 'description',
            });
            fields.appendChild(description);
            let slot = get_property(event, "description");
            slot.push([description, (s, v) => s.innerHTML = v]);
        }

        let textarea = makeElement('textarea', {
            name: "description",
            placeholder: "Description (optional)",
            innerHTML: description.innerText,
            required: false,
        });

        textarea.oninput = function () {
            event.properties["description"] = this.value;
        }

        let slot = get_property(event, "description");
        let idx = slot.findIndex(e => e[0] === description);
        slot.splice(idx, 1, [input, (s, v) => s.innerHTML = v])

        description.replaceWith(textarea);
    }

    /* ---------------------------------------- */

    let evtext = popup.getElementsByClassName('eventtext')[0]
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
    evtext.prepend(calendar_dropdown);

    /* ---------------------------------------- */

    let submit = makeElement( 'input', {
        type: 'submit',
        value: 'Skapa event',
    });

    let article = popup.getElementsByClassName("eventtext")[0];
    article.appendChild(submit);


    let wrappingForm = makeElement('form', {
        onsubmit: function (e) {
            create_event(event);
            return false;
        }});
    article.replaceWith(wrappingForm);
    wrappingForm.appendChild(article);

	/* this is for existing events.
	 * Newly created events aren't in the DOM tree yet, and can
	 * therefore not yet be focused */
	input.focus();

}

window.onload = function () {
    start_time.setTime(document.querySelector("meta[name='start-time']").content * 1000)
    end_time.setTime(document.querySelector("meta[name='end-time']").content * 1000)

    update_current_time_bar()
    // once a minute for now, could probably be slowed to every 10 minutes
    window.setInterval(update_current_time_bar, 1000 * 60)

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

    /*
    xml.querySelector("summary text").innerHTML = "Pastahack";
    let serializer = new XMLSerializer();
    serializer.serializeToString(xml);
    */


    for (let el of document.querySelectorAll(".inline-edit input")) {
        el.oninput = update_inline_list;
    }


    for (let el of document.getElementsByClassName("newfield")) {
        let [name, type_selector, value_field] = el.children;

        /* TODO list fields */
        /* TODO add and remove fields. See update_inline_list */

        function update_value_field (el) {
            let [name_field, type_selector, value_field] = el.children;


            let value = makeElement('input');
            let values = [value];


            switch (name_field.value.toUpperCase()) {
            case 'GEO':
                value.type = 'number';
                values.push(makeElement('input', {
                    type: 'number',
                }));
                break;

            case 'CLASS':
                // Add auto completion
                break;

            case 'ACTION':
                // Add auto completion
                break;

            case 'TRANSP':
                // Replace with toggle betwen OPAQUE and TRANSPARENT
                break;

            case 'PERCENT-COMPLETE':
                value.min = 0;
                value.max = 100;
                break;

            case 'PRIORITY':
                value.min = 0;
                value.max = 9;
                break;

            default:


                switch (type_selector.options[type_selector.selectedIndex].value) {
                case 'integer':
                case 'float':
                    value.type = 'number';
                    break;

                case 'uri':
                    value.type = 'url';
                    break;

                case 'binary':
                    value.type = 'file';
                    break;

                case 'date-time':
                    values.push(makeElement('input', {
                        type: 'time',
                    }));
                    /* fallthrough */
                case 'date':
                    value.type = 'date';
                    break;

                case 'cal-address':
                    value.type = 'email';
                    break;

                case 'utc-offset':
                    value.type = 'time';
                    let lbl = makeElement('label');
                    let id = gensym();

                    lbl.setAttribute('for', id);

                    /* TODO make these labels stand out more */
                    lbl.appendChild(makeElement('span', {
                        className: 'plus',
                        innerText: '+',
                    }));
                    lbl.appendChild(makeElement('span', {
                        className: 'minus',
                        innerText: '-',
                    }));
                    values.splice(0,0,lbl);
                    values.splice(0,0, makeElement('input', {
                        type: 'checkbox',
                        style: 'display:none',
                        className: 'plusminuscheck',
                        id: id,
                    }));
                    break;

                case 'boolean':
                    value.type = 'checkbox';
                    break;

                case 'period':
                    value.type = 'text';
                    // TODO validate /P\d*H/ typ
                    break;

                case 'recur':
                    // TODO
                default:
                    value.type = 'text';
                }
            }


            value_field.innerHTML = '';
            for (let v of values) {
                console.log(v);
                value_field.appendChild(v);
            }
        }

        name.oninput = function () {
            let types = valid_input_types[this.value.toUpperCase()];
            type_selector.disabled = false;
            if (types) {
                type_selector.innerHTML = '';
                for (let type of types) {
                    type_selector.appendChild(
                        makeElement('option', { value: type, innerText: type }))
                }
                if (types.length == 1) {
                    type_selector.disabled = true;
                }
            } else {
                type_selector.innerHTML = '';
                for (let type of all_types) {
                    type_selector.appendChild(
                        makeElement('option', { value: type, innerText: type }))
                }
            }

            update_value_field(el);
        }
        type_selector.onchange = function () {
            update_value_field(el);
        }
    }

}

function close_popup(popup) {
    popup.classList.remove("visible");
}

function open_popup(popup) {
    popup.classList.add("visible");
    let element = document.getElementById(popup.id.substr(5))
    // let root = document.body;
    let root;
    switch (VIEW) {
    case 'week':
        root = document.getElementsByClassName("days")[0];
        break;
    case 'month':
    default:
        root = document.body;
        break;
    }
    /* start <X, Y> sets offset between top left corner
       of event in calendar and popup. 10, 10 soo old
       event is still visible */
    let offsetX = 10, offsetY = 10;
    while (element !== root) {
        offsetX += element.offsetLeft;
        offsetY += element.offsetTop;
        element = element.offsetParent;
    }
    popup.style.left = offsetX + "px";
    popup.style.top = offsetY + "px";
}

function toggle_popup(popup_id) {
    let popup = document.getElementById(popup_id);
    if (popup.classList.contains("visible")) {
        close_popup(popup);
    } else {
        open_popup(popup);
    }
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
        el.properties = {};
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
class display_tab {
}

class edit_tab {
}

class vcomponent {
    set_value(field, value) {
        if (value === '') {
            remove_property(field);
        }
    }
}


*/

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
    let popup = document.getElementById("popup" + el.id);
    // let children = el.getElementsByTagName("properties")[0].children;

    /* actual component (not popup) */
    for (let e of el.querySelectorAll(".bind")) {
        let f = ((s, v) => s.innerHTML = v.format(s.dataset && s.dataset.fmt));
        get_property(el, e.dataset.property).push([e, f]);
    }

    /* primary display tab */

    for (let e of popup.querySelectorAll(".summary-tab .bind")) {
        let f = (s, v) => s.innerHTML = v.format(s.dataset && s.dataset.fmt);
        get_property(el, e.dataset.property).push([e, f]);
    }

    /* edit tab */
    for (let e of popup.querySelectorAll(".edit-tab .bind")) {
        let p = get_property(el, e.dataset.property);
        e.oninput = function () {
            el.properties[e.dataset.property] = this.value;
        }
        let f;
        switch (e.tagName) {
        case 'input':
            switch (e.type) {
            case 'time': f = (s, v) => s.value = v.format("~H:~M:~S"); break;
            case 'date': f = (s, v) => s.value = v.format("~Y-~m-~d"); break;
            // TODO remaining types cases
            default: f = (s, v) => s.value = v;
            }
            p.push([e, f])
            break;
        case 'textarea':
            f = (s, v) => s.innerHTML = v;
            p.push([e, f])
            break;
        default:
            alert("How did you get here??? " + e.tagName)
            break;
        }
    }


    for (let field of ['dtstart', 'dtend']) {
        get_property(el, `--${field}-time`).push(
            [el, (el, v) => { let date = el.properties.dtstart;
                             let [h,m,s] = v.split(':')
                             date.setHours(Number(h));
                             date.setMinutes(Number(m));
                             el.properties[field] = date; }])
        get_property(el, `--${field}-date`).push(
            [el, (el, v) => { let date = el.properties.dtstart;
                             let [y,m,d] = v.split('-')
                             date.setYear(Number(y) - 1900);
                             date.setMinutes(Number(m) - 1);
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
                            .value = v.format("~H:~M:~S");
                            popup
                            .querySelector(`.edit-tab input[name='${field}-date']`)
                            .value = v.format("~Y-~m-~d");
                            }]);
    }


    for (let child of el.querySelector("vevent > properties").children) {
        let field = child.tagName;

        let lst = get_property(el, field);

        /* Bind vcomponent fields for this event */
        for (let s of el.querySelectorAll(`${field} > :not(parameters)`)) {
            switch (s.tagName) {
            case 'date':
                lst.push([s, (s, v) => s.innerHTML = v.format("~Y-~m-~d")]); break;
            case 'date-time':
                lst.push([s, (s, v) => s.innerHTML = v.format("~Y-~m-~dT~H:~M:~S~Z")]); break;
            default:
                lst.push([s, (s, v) => s.innerHTML = v]);
            }
            el.properties["_value_" + field] = s.innerHTML;
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
        el.properties.dtstart = parseDate(el.properties.dtstart);
        get_property(el, 'dtstart').push(
            [el.style, (s, v) =>
             s[wide_event?'left':'top'] = 100 * (to_local(v) - start)/(end - start) + "%"]);
    }


    if (el.properties.dtend) {
        el.properties.dtend = parseDate(el.properties.dtend);
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


function advance_final(li) {
    li.classList.remove("final");
    let new_li = makeElement ('input', {
        className: 'final',
        size: 2,
        oninput: li.oninput,
    });
    li.closest(".inline-edit").appendChild(new_li);
}

function update_inline_list () {
    if (this.classList.contains("final")) {
        if (this.value !== '') {
            advance_final(this);
        }
    } else {
        if (this.value === '') {
            let sibling = this.previousElementSibling || this.nextElementSibling;
            this.remove();
            sibling.focus();
        }
    }
}
