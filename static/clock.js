
class Clock {
    update(now) {
    }
}


class Timebar extends Clock {

    constructor(start_time, end_time) {
        super();
        this.start_time = start_time;
        this.end_time = end_time;
        this.bar_object = false
    }


    update(now) {
        // if (! (this.start_time <= now.getTime() && now.getTime() < this.end_time))
        //     return;

        var event_area = document.getElementById(now.format("~Y-~m-~d"))

        if (event_area) {
            if (this.bar_object) {
                this.bar_object.parentNode.removeChild(this.bar_object)
            } else {
                this.bar_object = makeElement ('div', {
                    id: 'bar',
                    className: 'eventlike current-time',
                });
            }

            this.bar_object.style.top = date_to_percent(now) + "%";
            event_area.append(this.bar_object)
        }
    }
}

class SmallcalCellHighlight extends Clock {
    constructor(small_cal) {
        super();
        this.small_cal = small_cal;
        this.current_cell = false
    }

    update(now) {
        if (this.current_cell) {
            this.current_cell.style.border = "";
        }

        /* This is expeced to fail if the current date is not
           currently on screen. */
        this.current_cell = this.small_cal.querySelector(
            "time[datetime='" + now.format("~Y-~m-~d") + "']");

        if (this.current_cell) {
            this.current_cell.style.border = "1px solid black";
        }
    }
}

/* -------------------------------------------------- */

class ClockElement extends HTMLElement {
    constructor () {
        super();
    }

    connectedCallback () {
        let interval = this.hasAttribute('interval') ? +this.getAttribute('img') : 60;
        interval *= 1000 /* ms */

        this.timer_id = window.setInterval(() => this.update(new Date), interval)
        this.update(new Date)
    }

    static get observerAttributes () {
        return ['timer_id']
    }

    update (now) { /* noop */ }
}

class TodayButton extends ClockElement {
    update (now) {
        this.querySelector('a').href = now.format("~Y-~m-~d.html")
    }
}
customElements.define('today-button', TodayButton)


class CurrentTime extends ClockElement {
    update (now) {
        this.innerHTML = now.format('~H:~M:~S')
    }
}
customElements.define('current-time', CurrentTime)
