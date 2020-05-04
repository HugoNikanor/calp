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
    var minute = String((time - Math.floor(time)) * 60).padStart(2, 0);
    return "" + hour + ":" + minute;
}

/* start and end time for calendar page */
let start_time = new Date();
let end_time = new Date();

/* dynamicly created event when dragging */
let event;

function create_event_down (e) {
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

    /* Makes all current events transparent when dragging over them.
       Without this weird stuff happens when moving over them
    */
    for (let e of this.children) {
        e.style.pointerEvents = "none";
    }

    this.appendChild(event);
}

function create_event_move (e) {
    if (! event) return;

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

        callback (event, start, end);

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

function setVar(str, val) {
	document.documentElement.style.setProperty("--" + str, val);
}

function close_all_popups () {
	for (let popup of document.getElementsByClassName("popup")) {
		popup.classList.remove("show");
	}
}

window.onload = function () {
    start_time.setTime(document.querySelector("meta[name='start-time']").content * 1000)
    end_time.setTime(document.querySelector("meta[name='end-time']").content * 1000)

    update_current_time_bar()
    // once a minute for now, could probably be slowed to every 10 minutes
    window.setInterval(update_current_time_bar, 1000 * 60)

    /* Is event creation active? */
    if (false) {
        for (let c of document.getElementsByClassName("events")) {
            c.onmousedown = create_event_down;
            c.onmousemove = create_event_move;
            c.onmouseup = create_event_finisher(
                function (event, start, end) {
                    let startstr = decimal_time_to_string(start);
                    let endstr = decimal_time_to_string(end);

                    alert("Creating event " + startstr + " - " + endstr);
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
}

$(document).ready(function() {
    Tipped.setDefaultSkin("purple");
    Tipped.create(".event", {
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
    });
});
