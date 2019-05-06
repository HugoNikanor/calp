
function part_to_hour (f) {
    return Math.floor(10 * 24 * f) / 10;
}

var start_time = 0
var start_fraq = 0

var parent
var createdEvents = false

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

window.onload = function () {
    for (let c of document.getElementsByClassName("events")) {
        c.onmousedown = onmousedownhandler;
        c.onmouseup = onmouseuphandler;
        c.onmousemove = onmousemovehandler;
    }
}
