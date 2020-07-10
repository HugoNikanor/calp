/* -------------------------------------------------- */

function round_time (time, fraction) {
    let scale = 1 / fraction;
    return Math.round (time * scale) / scale;
}

function time_to_percent (time) {
    // Decimal time
    return (time.getHours() + (time.getMinutes() / 60)) * 100/24 + "%"
}

function parents_until (element, obj) {
    if (element === null) {
        return null;
    } else if (element.id == obj.id || element.classList.contains(obj.class)) {
        return element;
    } else {
        return parents_until (element.parentElement, obj);
    }
}

function decimal_time_to_string (time) {
    let hour = Math.floor(time);
    if (hour < 10) {
        hour = '0' + hour;
    }
    var minute = String((time - Math.floor(time)) * 60).padStart(2, 0);
    return "" + hour + ":" + minute;
}

let gensym_counter = 0;
function gensym (prefix) {
    gensym_counter++;
    return prefix + gensym_counter;
}

/* start and end time for calendar page */
let start_time = new Date();
let end_time = new Date();

/* dynamicly created event when dragging */
let event;

let event_start = { x: NaN, y: NaN };

let down_on_event = false;

function create_event_down (e) {
    /* Only trigger event creation stuff on actuall events background,
       NOT on its children */
    down_on_event = false;
    if (! e.target.classList.contains("events")) return;
    down_on_event = true;

    event_start.x = e.clientX;
    event_start.y = e.clientY;
}

function create_event_move (e) {

    if (e.buttons != 1 || ! down_on_event) return;

    /* Create event when we start moving the mouse. */
    if (! event) {
        /* Small deadzone so tiny click and drags aren't registered */
        if (abs(event_start.x - e.clientX) < 10
            && abs(event_start.y - e.clientY) < 5)
        { return; }

        /* only allow start of dragging on background */
        if (e.target != this) return;

        /* only on left click */
        if (e.buttons != 1) return;

        /* [0, 1) -- where are we in the container */
        /* Ronud to force steps of quarters */
        time = round_time(24 * (e.offsetY / this.clientHeight),
                          .25)

        event = document.createElement("div");
        event.style.top = time * 100/24 + "%";
        event.dataset.time1 = time;
        event.dataset.time2 = time;

        event.className = "event generated";
        event.style.pointerEvents = "none";
        event.style.width = "calc(100% * var(--editmode))";
        event.innerText = "New Event";

        event.dataset.date = this.id;

        /* Makes all current events transparent when dragging over them.
           Without this weird stuff happens when moving over them
        */
        for (let e of this.children) {
            e.style.pointerEvents = "none";
        }

        this.appendChild(event);
    }

    time2 = event.dataset.time2 =
        round_time(24 * (e.offsetY / event.parentElement.clientHeight),
                   .25);
    time1 = event.dataset.time1;
    if (time2 > time1) {
        event.style.bottom = (24 - time2) * 100/24 + "%";
        event.style.top = time1 * 100/24 + "%";
    } else {
        event.style.top = time2 * 100/24 + "%";
        event.style.bottom = (24 - time1) * 100/24 + "%";
    }
}

function create_event_finisher (callback) {
    down_on_event = false; // reset
    return function create_event_up (e) {
        if (! event) return;
        let start = min(Number(event.dataset.time1), Number(event.dataset.time2));
        let end = max(Number(event.dataset.time1), Number(event.dataset.time2));

        /* Restore pointer events for all existing events.
           Allow pointer events on our new event
        */
        for (let e of event.parentElement.children) {
            e.style.pointerEvents = "";
        }

        let localevent = event;
        event = null;

        callback (localevent, start, end);

    }
}

// for debugging
let last_xml;

async function remove_event (element) {
    console.log(element);
    let xmltext = element.getElementsByClassName("xcal")[0].innerText;
    let parser = new DOMParser();
    let xml = parser.parseFromString(xmltext, "text/xml");

    // for debugging
    last_xml = xml;

    let uid = xml.querySelector("uid").textContent.trim()

    let data = new URLSearchParams();
    data.append('uid', uid);

    let response = await fetch ( '/remove', {
        method: 'POST',
        body: data
    });

    console.log(response);
}

function time_to_date (time) {
    return [ time.getFullYear(),
             String(time.getMonth() + 1).padStart(2, '0'),
             String(time.getDate()).padStart(2, '0') ].join("-");
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

    var event_area = document.getElementById(time_to_date(now))

    if (event_area) {
        if (bar_object) {
            bar_object.parentNode.removeChild(bar_object)
        } else {
            bar_object = document.createElement("div")
            bar_object.className = "event current-time"
            bar_object.id = "bar"
        }

        bar_object.style.top = time_to_percent(now)
        event_area.append(bar_object)
    }

    /* */

    if (current_cell) {
        current_cell.style.border = "";
    }
    current_cell = document.querySelector(
        ".small-calendar time[datetime='" + time_to_date(now) + "']");
    current_cell.style.border = "1px solid black";

    /* Update [today] button */

    document.getElementById("today-button").href
        = time_to_date(new Date) + ".html";
}

function min(a, b) {
    return a < b ? a : b;
}

function max(a, b) {
    return a > b ? a : b;
}

function abs(n) {
    return n < 0 ? - n : n;
}

function setVar(str, val) {
	document.documentElement.style.setProperty("--" + str, val);
}

function close_all_popups () {
    for (let popup of document.querySelectorAll(".popup-container.visible")) {
        close_popup(popup);
    }
}

function sxml_to_xml(doc, tree) {

    if (typeof(tree) == 'string') return tree;

    let [tag, ...body] = tree;
    let node = doc.createElement(tag);
    for (const child of body) {
        node.append(sxml_to_xml(doc, child));
    }
    return node;
}

// FormData
async function create_event (date, fd) {

    let tree = ['vevent',
                ['properties',
                 ['dtstart', ['date-time', date + "T" + fd.get("dtstart") + ":00"]],
                 ['dtend', ['date-time', date + "T" + fd.get("dtend") + ":00"]],
                 ['summary', ['text', fd.get("summary")]]]]

    if (fd.get("description")) {
        tree[1].push(['description', ['text', fd.get("description")]])
    }

    let xmldoc = document.implementation.createDocument("", "", null)
    xml = sxml_to_xml(xmldoc, tree).outerHTML;

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

window.onload = function () {
    start_time.setTime(document.querySelector("meta[name='start-time']").content * 1000)
    end_time.setTime(document.querySelector("meta[name='end-time']").content * 1000)

    update_current_time_bar()
    // once a minute for now, could probably be slowed to every 10 minutes
    window.setInterval(update_current_time_bar, 1000 * 60)

    /* Is event creation active? */
    if (true) {
        for (let c of document.getElementsByClassName("events")) {
            c.onmousedown = create_event_down;
            c.onmousemove = create_event_move;
            c.onmouseup = create_event_finisher(
                function (event, start, end) {
                    let startstr = decimal_time_to_string(start);
                    let endstr = decimal_time_to_string(end);

                    let popupElement = document
                        .getElementById("popup-template")
                        .content
                        .cloneNode(true)
                        .firstChild ;
                    let id = gensym("popup-generated-");
                    popupElement.id = id
                    document.getElementsByClassName("root")[0].appendChild(popupElement);

                    popupElement.querySelector('input[name="dtstart"]').value = startstr;
                    popupElement.querySelector('input[name="dtend"]').value = endstr;

                    let form = popupElement.querySelector("form")
                    form.addEventListener("submit", function (ev) {
                        ev.preventDefault();
                        create_event(event.dataset.date, new FormData(form));
                    });

                    open_popup(popupElement);


                    /*
                    console.log(event);

                    alert("Creating event " + startstr + " - " + endstr);
                    */
                    popupElement.querySelector('input[name="summary"]').focus();
                });
        }
    }

    for (let el of document.getElementsByClassName("event")) {
        /* Popup script replaces need for anchors to events.
           On mobile they also have the problem that they make
           the whole page scroll there.
        */
        el.parentElement.removeAttribute("href");

        /* Bind all vcomponent properties into javascript. */
        el.properties = {}
        let children = el.getElementsByTagName("properties")[0].children;

        for (let child of children) {
            let field = child.tagName;


            lst = el.properties["_slot_" + field] = []
            for (let s of el.getElementsByClassName(field)) {
                let f = ((s, v) => s.innerHTML = v.format(s.dataset && s.dataset.fmt));
                lst.push([s, f]);
            }
            for (let s of el.querySelectorAll(field + " > :not(parameters)")) {
                lst.push([s, (s, v) => s.innerHTML = v]);
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

        el.properties.dtstart = new Date(el.properties.dtstart);

        el.properties["_slot_dtstart"].push(
            [el.style, (s, v) => s.top = time_to_percent(v)]);

    }

    document.onkeydown = function (evt) {
	evt = evt || window.event;
        if (! evt.key) return;
	if (evt.key.startsWith("Esc")) {
	    close_all_popups();
	}
    }


    /* Replace backend-driven [today] link with frontend, with one that gets
    correctly set in the frontend.
    Similarly, update the go to specific date button into a link which updates
    wheneven the date form updates.
    */

    let jumpto = document.getElementsByClassName("jump-to")[0];
    let gotodatebtn = jumpto.getElementsByTagName("button")[0];
    let golink = document.createElement("a");
    golink.classList.add("btn");
    let target_href = time_to_date(new Date) + ".html";
    document.getElementById("today-button").href = target_href;
    golink.href = target_href;
    golink.innerHTML = gotodatebtn.innerHTML;
    gotodatebtn.replaceWith(golink);

    jumpto.getElementsByTagName("input")[0].onchange = function () {
        let date = time_to_date(this.valueAsDate)
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


function format_date(date, str) {
    let fmtmode = false;
    let outstr = "";
    for (var i = 0; i < str.length; i++) {
        if (fmtmode) {
            switch (str[i]) {
            case 'H': outstr += (date.getHours() + "").padStart(2, "0"); break;
            case 'M': outstr += (date.getMinutes() + "").padStart(2, "0"); break;
            case 'S': outstr += (date.getSeconds() + "").padStart(2, "0"); break;
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
