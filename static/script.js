
function part_to_hour (f) {
    return Math.floor(10 * 24 * f) / 10;
}

function hour_to_part (hour) {
    return 100 * (hour / 24)
}

function time_to_percent (time) {
    // Decimal time
    return hour_to_part(time.getHours() + (time.getMinutes() / 60)) + "%"
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

let start_time = new Date();
let end_time = new Date();

var event_start_time = 0
var start_fraq = 0

var parent
var createdEvent = false

function onmousedownhandler (e) {
    last = this
    event = e
    var comp = this
    console.log(comp.clientHeight)
    while (! comp.classList.contains("events")) {
        comp = comp.parentElement
    }
    console.log(e);
    parent = comp
    console.log(comp.clientHeight)
    fraq = e.offsetY / comp.clientHeight
    start_fraq = fraq
    event_start_time = part_to_hour(fraq);
    createdEvent = document.createElement("div");
    createdEvent.className = "event generated";
    createdEvent.style.pointerEvents = "none";
    createdEvent.style.width = "100%";
    createdEvent.style.top = fraq * 100 + "%";
    createdEvent.innerText = "New Event";
}

function onmousemovehandler (e) {
    if (createdEvent) {
        fraq = e.offsetY / this.clientHeight
        var diff = fraq - start_fraq;
        if (! createdEvent.parentElement) {
            for (let e of this.children) {
                e.style.pointerEvents = "none";
            }
            this.appendChild(createdEvent);
        }
        createdEvent.style.height = diff * 100 + "%";
    }

}

function onmouseuphandler (e) {
    var end_time = part_to_hour(e.offsetY / this.clientHeight);
    console.log("Creating event " + event_start_time + " - " + end_time);
    createdEvent = false;

    for (let e of parent.children) {
        e.style.pointerEvents = "initial";
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
}

function toggle_event_pupup () {
    console.log(this);
    this.getElementsByClassName("popup")[0].classList.toggle("show");
}

let days;

function min(a, b) {
    a < b ? a : b;
}

function max(a, b) {
    a > b ? a : b;
}

function close_popup (btn) {
    o = { class: "popup" }
    var popup = parents_until (btn, o);
    popup.classList.toggle("show");
}

/*
https://stackoverflow.com/questions/21064101/understanding-offsetwidth-clientwidth-scrollwidth-and-height-respectively
*/
function new_popup (event) {

    console.log(event.target);
    console.log(this);
    // if (event.target !== this) return;

    /* popup = this.children[0].children[0] */
    let ev = parents_until(this, {class: "event"})
    popup = ev.getElementsByClassName("popup")[0];
    popup.classList.toggle("show")

    return;

    /* x-axis fix */

    /* Popup should neven be wider than viewport */
    popup.style.width = min(popup.style.offsetWidth, days.clientWidth - 10) + "px"

    /* find left edge of source element in viewport */
    here_x = (this.offsetParent.offsetLeft + this.offsetLeft) - days.scrollLeft;
    console.log(here_x)

    /* Align popup with source */
    popup.style.left = "0px"
    /* move it if it would partially render outside */
    if (here_x < 0) {
        popup.style.left = "" + ((- here_x) + 10) + "px";
    }
    // overflow_x = (here_x + popup.offsetWidth) - days.offsetWidth
    overflow_x = (here_x + popup.offsetWidth) - days.clientWidth
    if (overflow_x > 0) {
        /* NOTE
           Setting the style.left field changes the offsetWidth
           of the object.
           TODO
           Get the popup to actually be inside the viewport!
           */
        // console.log(overflow_x)
        popup.style.left = "-" + (overflow_x + 10) + "px";
    }

    /* y-axis fix */
    popup.style.height = min(popup.style.offsetHeight, days.offsetHeight)
    popup.style.bottom = "calc(100% + 2em)";

    here_y = this.offsetParent.offsetTop + this.offsetTop;

    overflow_y = here_y - popup.offsetHeight;
    if (overflow_y < 0) {
        popup.style.bottom = "calc(100% - " + ((- overflow_y) + 10) + "px)";
    }

}

function setVar(str, val) {
	document.documentElement.style.setProperty("--" + str, val);
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
            c.onmousedown = onmousedownhandler;
            c.onmouseup = onmouseuphandler;
            c.onmousemove = onmousemovehandler;
        }
    }

    // for (let e of document.getElementsByClassName("event-inner")) {
    //     e.onclick = toggle_event_pupup;
    // }

    days = document.getElementsByClassName("days")[0]
    for (let popup of document.getElementsByClassName("popup")) {
        ev = parents_until(popup, {class: "event"})
        e = ev.getElementsByClassName("body")[0]
        e.onclick = new_popup;
        /* disable scroll to element in side list
           if popups are available.
        */
        e.parentElement.removeAttribute("href");
    }

    // days.scrollLeft == 0
    // days.offsetWidth == viewable width
    // days.offsetHeight == viewable height

}
