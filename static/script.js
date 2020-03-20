
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

var start_time = 0
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
    start_time = part_to_hour(fraq);
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
    console.log("Creating event " + start_time + " - " + end_time);
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

function update_current_time_bar () {
    var now = new Date()
    var today = document
        .getElementById(time_to_date(now))
        .parentElement
        .parentElement

    var event_area = today.getElementsByClassName("events")[0]

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

function toggle_event_pupup () {
    console.log(this);
    this.getElementsByClassName("popup")[0].classList.toggle("show");
}

window.onload = function () {
    console.log("Running");

    /*
    update_current_time_bar()
    // once a minute for now, could probably be slowed to every 10 minutes
    window.setInterval(update_current_time_bar, 1000 * 60)

    for (let c of document.getElementsByClassName("events")) {
        c.onmousedown = onmousedownhandler;
        c.onmouseup = onmouseuphandler;
        c.onmousemove = onmousemovehandler;
    }
    */

    for (let e of document.getElementsByClassName("event-inner")) {
        e.onclick = toggle_event_pupup;
    }
}
