/* ------- XML Formatting --------------------------- */

function start_tag(str) {
    return "<span class='html-tag'>&lt;" + str + "&gt;</span>";
}

function end_tag(str) {
    return "<span class='html-tag'>&lt;" + str + "/&gt;</span>";
}

function istring(indent) {
    return "".padStart(indent);
}

function pretty_print_xml(xml, indent=0) {
    /* pretty_print_xml(xml.documentElement) */

    let tag = xml.tagName;


    if (xml.childElementCount == 0) {
        return istring(indent)
            + start_tag(tag)
            + "<b>" + xml.textContent + "</b>"
            + end_tag(tag);
    } else {
        let str = istring(indent) + start_tag(tag) + "<br/>";
        for (let child of xml.children) {
            str += pretty_print_xml(child, indent + 1) + "<br/>";
        }
        str += istring(indent) + end_tag(tag);
        return str;
    }
}

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

function create_event_down (e) {
    event_start.x = e.clientX;
    event_start.y = e.clientY;
}

function create_event_move (e) {

    if (e.buttons != 1) return;

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
	for (let popup of document.getElementsByClassName("tpd-tooltip")) {
		popup.getElementsByClassName('close-tooltip')[0].click();
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
        tree.push(['description', ['text', fd.get("description")]])
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

                    event.dataset.tippedOptions = "inline: '" + id + "'";
                    Tipped.create(event, tipped_args);
                    event.click();

                    /*
                    console.log(event);

                    alert("Creating event " + startstr + " - " + endstr);
                    */
                    popupElement.querySelector('input[name="summary"]').focus();
                });
        }
    }

    /* Popup script replaces need for anchors to events.
       On mobile they also have the problem that they make
       the whole page scroll there.
    */
    for (let el of document.getElementsByClassName("event")) {
        el.parentElement.removeAttribute("href");

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

    /* Pretty prints the xcal contents in each popup.
       Done here since guile produces compact xml.

       element.innerText should still be valid xcal xml.
    */

    let parser = new DOMParser();
    for (let el of document.querySelectorAll("[type='application/calendar+xml']")) {
        let xml = parser.parseFromString(el.innerText, "text/xml");
        el.outerHTML = "<pre class='xcal'>"
            + pretty_print_xml(xml.documentElement, 0)
            + "</pre>";


    }
}


let tipped_args = {
    /* no padding, I am the one who styles! */
    padding: false,
    /* Don't remove from DOM when hiding */
    detach: false,

    /* click element to toggle.
       Elements with class "close-tooltip" also
       acts as close buttons */
    showOn: 'click',
    hideOn: 'click',

    /* makes popups relative our scrolling days view */
    container: '.days',

    /* Ensures that the popups stay within the given area,
       and don't strectch the container */
    containment: {
        selector: '.days',
    },
    behaviour: 'sticky',
}

$(document).ready(function() {
    Tipped.setDefaultSkin("purple");
    Tipped.create(".event", tipped_args);
});

