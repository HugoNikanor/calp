'use strict';

function makeElement (name, attr={}) {
    let element = document.createElement(name);
    for (let [key, value] of Object.entries(attr)) {
        element[key] = value;
    }
    return element;
}

/* -------------------------------------------------- */

function round_time (time, fraction) {
    let scale = 1 / fraction;
    return Math.round (time * scale) / scale;
}

function date_to_percent (date) {
    // Decimal time
    return (date.getHours() + (date.getMinutes() / 60)) * 100/24;
}

function decimal_time_to_date (time, date) {
    let hour = Math.floor(time);
    let minute = (time - hour) * 60;
    if (date) {
        return new Date(date.getFullYear(),
                        date.getMonth(),
                        date.getDate(),
                        hour, minute, 0);
    } else {
        return new Date(0,0,0,hour,minute,0);
    }
}

/* if only there was such a thing as a let to wrap around my lambda... */
const gensym = (counter => (prefix="gensym") => prefix + ++counter)(0)

/* start and end time for calendar page */
let start_time = new Date();
let end_time = new Date();

class EventCreator {

    /* dynamicly created event when dragging */
    constructor() {
        this.event = false;
        this.event_start = { x: NaN, y: NaN };
        this.down_on_event = false;
    }

    create_event_down () {
        let that = this;
        return function (e) {
            /* Only trigger event creation stuff on actuall events background,
               NOT on its children */
            that.down_on_event = false;
            if (! e.target.classList.contains("events")) return;
            // if (! e.target.classList.contains("longevents")) return;
            that.down_on_event = true;

            that.event_start.x = e.clientX;
            that.event_start.y = e.clientY;
        }
    }

    create_event_move() {
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


                let event
                    = that.event
                    = document.getElementById("event-template")
                      .firstChild.cloneNode(true);
                bind_properties(event);

                /* [0, 1) -- where are we in the container */
                /* Ronud to force steps of quarters */
                let time = round_time(24 * (e.offsetY / this.clientHeight),
                                      .25);
                /*
                  time = round_time((e.offsetX / this.clientWidth),
                  1/(7*(24/8)));
                */

                event.style.top = time * 100/24 + "%";
                event.dataset.time1 = time;
                event.dataset.time2 = time;

                event.style.pointerEvents = "none";

                /* ---------------------------------------- */

                place_in_edit_mode(event);

                /* ---------------------------------------- */


                event.dataset.date = this.id;

                /* Makes all current events transparent when dragging over them.
                   Without this weird stuff happens when moving over them
                */
                for (let e of this.children) {
                    e.style.pointerEvents = "none";
                }

                this.appendChild(event);
            }

            let time2 = that.event.dataset.time2 =
                round_time(24 * (e.offsetY / that.event.parentElement.clientHeight),
                           .25);
            let time1 = that.event.dataset.time1;

            let date = new Date(that.event.dataset.date)
            that.event.properties.dtstart =
                decimal_time_to_date(Math.min(Number(time1), Number(time2)),
                                     date);
            that.event.properties.dtend =
                decimal_time_to_date(Math.max(Number(time1), Number(time2)),
                                     date);


            /*
    time2 = event.dataset.time2 =
        round_time((e.offsetX / event.parentElement.clientWidth),
                   1/(7*(24/8)));
        // round_time(24 * (e.offsetX / event.parentElement.clientWidth),
        //            .25);
    time1 = Number(event.dataset.time1);

    // let date = new Date(event.dataset.date)
    let d1 = new Date(start_time.getTime() + (end_time-start_time) * Math.min(time1,time2));
    let d2 = new Date(start_time.getTime() + (end_time-start_time) * Math.max(time1,time2));
    event.properties.dtstart = d1;
        // decimal_time_to_date(Math.min(Number(time1), Number(time2)),
        //                      date);
    event.properties.dtend = d2;
        // decimal_time_to_date(Math.max(Number(time1), Number(time2)),
        //                      date);
        */
        }
    }

    create_event_finisher (callback) {
        this.down_on_event = false; // reset
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
}

var bar_object = false
var current_cell = false

function update_current_time_bar () {
    var now = new Date()
    /* TODO
       The bar and box doesn't get cleared when we leave our time interval.
    */
    if (! (start_time <= now.getTime() && now.getTime() < end_time))
        return;

    var event_area = document.getElementById(now.format("%Y-%m-%d"))

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
        ".small-calendar time[datetime='" + now.format("%Y-%m-%d") + "']");
    current_cell.style.border = "1px solid black";

    /* Update [today] button */

    document.getElementById("today-button").href
        = (new Date).format("%Y-%m-%d") + ".html";
}

function setVar(str, val) {
	document.documentElement.style.setProperty("--" + str, val);
}

function close_all_popups () {
    for (let popup of document.querySelectorAll(".popup-container.visible")) {
        close_popup(popup);
    }
}

async function create_event (event) {

    let xml = event.getElementsByTagName("icalendar")[0].outerHTML

    console.log(xml);

    let data = new URLSearchParams();
    data.append("cal", "Calendar");
    data.append("data", xml);

    let response = await fetch ( '/insert', {
        method: 'POST',
        body: data
    });

    console.log(response);

    let body = await response.text();
    console.log(body);
}

function place_in_edit_mode (event) {
    function replace_with_time_input(fieldname, event) {
        let field = event.getElementsByClassName(fieldname)[0];

        let input = makeElement ('input', {
            type: "time",
            required: true,

            onchange: function (e) {
                let [hour, minute] = this.value.split(":").map(Number);
                /* retain the year, month, and day information */
                let d = new Date(event.properties[fieldname]);
                d.setHours(hour);
                d.setMinutes(minute);
                event.properties[fieldname] = d;
            }
        });
        let slot = event.properties["_slot_" + fieldname]
        let idx = slot.findIndex(e => e[0] === field);
        slot.splice(idx, 1, [input, (s, v) => s.value = v.format("%H:%M")])
        field.replaceWith(input);

    }

    /* TODO ensure dtstart < dtend */
    replace_with_time_input("dtstart", event);
    replace_with_time_input("dtend", event);

    /* ---------------------------------------- */

    let summary = event.getElementsByClassName("summary")[1];
    let input = makeElement('input', {
        name: "dtstart",
        placeholder: summary.innerText,
        required: true,
    });

    input.oninput = function () {
        event.properties["summary"] = this.value;
    }

    let slot = event.properties["_slot_summary"]
    let idx = slot.findIndex(e => e[0] === summary);
    slot.splice(idx, 1, [input, (s, v) => s.value = v])

    summary.replaceWith(input);

    /* ---------------------------------------- */

    let submit = makeElement( 'input', {
        type: 'submit',
        value: 'Skapa event',
    });

    let article = event.getElementsByTagName("article")[0];
    article.appendChild(submit);


    let wrappingForm = makeElement('form', {
        onsubmit: function (e) {
            create_event(event);
            return false;
        }});
    article.replaceWith(wrappingForm);
    wrappingForm.appendChild(article);

}

window.onload = function () {
    start_time.setTime(document.querySelector("meta[name='start-time']").content * 1000)
    end_time.setTime(document.querySelector("meta[name='end-time']").content * 1000)

    update_current_time_bar()
    // once a minute for now, could probably be slowed to every 10 minutes
    window.setInterval(update_current_time_bar, 1000 * 60)

    /* Is event creation active? */
    if (true) {
        let eventCreator = new EventCreator;
        for (let c of document.getElementsByClassName("events")) {
            c.onmousedown = eventCreator.create_event_down();
            c.onmousemove = eventCreator.create_event_move();
            c.onmouseup = eventCreator.create_event_finisher(
                function (event) {
                    let popupElement = event.querySelector(".popup-container");
                    open_popup(popupElement);

                    popupElement.querySelector("input[name='dtstart']").focus();

                });
        }
    }

    for (let nav of document.getElementsByClassName("popup-control")) {
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
            bind_properties(el);
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

    let jumpto = document.getElementsByClassName("jump-to")[0];
    let gotodatebtn = jumpto.getElementsByTagName("button")[0];
    let target_href = (new Date).format("%Y-%m-%d") + ".html";
    let golink = makeElement('a', {
        className: 'btn',
        href: target_href,
        innerHTML: gotodatebtn.innerHTML,
    });
    document.getElementById("today-button").href = target_href;
    gotodatebtn.replaceWith(golink);

    jumpto.getElementsByTagName("input")[0].onchange = function () {
        let date = this.valueAsDate.format("%Y-%m-%d");
        golink.href = date + ".html";
    }

    /* ---------------------------------------- */

    /*
    xml.querySelector("summary text").innerHTML = "Pastahack";
    let serializer = new XMLSerializer();
    serializer.serializeToString(xml);
    */

}

function close_popup(popup) {
    popup.classList.remove("visible");
}

function open_popup(popup) {
    popup.classList.add("visible");
}

function toggle_popup(popup) {
    popup.classList.toggle("visible");
}

function toggle_child_popup(el) {
    let popup = el.getElementsByClassName("popup-container")[0];
    toggle_popup(popup);
}

function datepad(thing, width=2) {
    return (thing + "").padStart(width, "0");
}

function format_date(date, str) {
    let fmtmode = false;
    let outstr = "";
    for (var i = 0; i < str.length; i++) {
        if (fmtmode) {
            switch (str[i]) {
            case 'Y': outstr += datepad(date.getFullYear(), 4); break;
            case 'm': outstr += datepad(date.getMonth() + 1);   break;
            case 'd': outstr += datepad(date.getDate());        break;
            case 'H': outstr += datepad(date.getHours());       break;
            case 'M': outstr += datepad(date.getMinutes());     break;
            case 'S': outstr += datepad(date.getSeconds());     break;
            }
            fmtmode = false;
        } else if (str[i] == '%') {
            fmtmode = true;
        } else {
            outstr += str[i];
        }
    }
    return outstr;
}
Object.prototype.format = function () { return this; } /* any number of arguments */
Date.prototype.format = function (str) { return format_date (this, str); }


function bind_properties (el, wide_event=true) {
    el.properties = {}
    let children = el.getElementsByTagName("properties")[0].children;

    for (let child of children) {
        let field = child.tagName;


        let lst = el.properties["_slot_" + field] = []
        for (let s of el.getElementsByClassName(field)) {
            let f = ((s, v) => s.innerHTML = v.format(s.dataset && s.dataset.fmt));
            lst.push([s, f]);
        }
        for (let s of el.querySelectorAll(field + " > :not(parameters)")) {
            switch (s.tagName) {
                // TODO TZ?
            case 'date':
                lst.push([s, (s, v) => s.innerHTML = v.format("%Y-%m-%d")]); break;
            case 'date-time':
                lst.push([s, (s, v) => s.innerHTML = v.format("%Y-%m-%dT%H:%M:%S")]); break;
            default:
                lst.push([s, (s, v) => s.innerHTML = v]);
            }
            /* TODO the icalendar data is only here when we have edit mode entabled.
               Either always include it, or find the default value some other way */
            el.properties["_value_" + field] = s.innerHTML;
        }

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

    if (el.properties.dtstart) {
        el.properties.dtstart = new Date(el.properties.dtstart);
        el.properties["_slot_dtstart"].push(
            [el.style,
             wide_event
             ? (s, v) => s.left = 100 * (v - start_time)/(end_time - start_time) + "%"
             : (s, v) => s.top = date_to_percent(v) + "%"]);
    }

    if (el.properties.dtend) {
        el.properties.dtend = new Date(el.properties.dtend);
        el.properties["_slot_dtend"].push(
            [el.style,
             wide_event
             // TODO right and bottom only works if used from the start. However,
             // events from the backend instead use top/left and width/height.
             // Normalize so all use the same, or find a way to convert between.
             ? (s, v) => s.right = 100 * (1 - (v-start_time)/(end_time-start_time)) + "%"
             : (s, v) => s.bottom = (100 - date_to_percent(v)) + "%"]);
    }
}
