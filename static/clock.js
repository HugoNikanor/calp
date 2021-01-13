
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

        this.current_cell = this.small_cal.querySelector(
            "time[datetime='" + now.format("~Y-~m-~d") + "']");

        this.current_cell.style.border = "1px solid black";
    }
}

/* Update [today] button */
class ButtonUpdater extends Clock {
    constructor(el, proc) {
        super();
        this.el = el;
        this.proc = proc;
    }

    update(now) {
        this.proc(this.el, now);
    }
}
