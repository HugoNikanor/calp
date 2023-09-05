/**
  * Components for working with things which depend on the current time.
  *
  * Also introduces two web components:
  *
  * ```html
  * <today-button />
  * <current-time />
  * ```
  *
  * TODO shouldn't these be defined with the rest of the components?
  *
  * @module
  */

export {
    SmallcalCellHighlight, Timebar,
    initialize_clock_components
}

import { makeElement, date_to_percent } from './lib'

/**
 * Interface for `things` which wants to get updated on a human timescale.
 */
export abstract class Clock {
    /** Called every now and then
     * @param now Called with the current time
     */
    abstract update(now: Date): void;
}

/** The (blue) vertical line which show the current time in the current day. */
class Timebar extends Clock {

    // start_time: Date
    // end_time: Date
    bar_object: HTMLElement | null

    constructor(/*start_time: Date, end_time: Date*/) {
        super();
        // this.start_time = start_time;
        // this.end_time = end_time;
        this.bar_object = null
    }

    update(now: Date) {
        // if (! (this.start_time <= now.getTime() && now.getTime() < this.end_time))
        //     return;

        var event_area = document.getElementById(now.format("~Y-~m-~d"))

        if (event_area) {
            if (this.bar_object !== null && this.bar_object.parentNode !== null) {
                this.bar_object.parentNode.removeChild(this.bar_object)
            } else {
                this.bar_object = makeElement('div', {
                    id: 'bar',
                    className: 'eventlike current-time',
                });
            }

            this.bar_object.style.top = date_to_percent(now) + "%";
            event_area.append(this.bar_object)
        }
    }
}

/**
 * Highlights the current date in the small calendar to the side.
 * Currently directly sets a border
 *
 * @TODO{but should preferably set a class instead}.
*/
class SmallcalCellHighlight extends Clock {

    small_cal: HTMLElement
    current_cell: HTMLElement | null

    /**
     * @param small_cal the DOM-node of the calendar widget. It must support
     *   querySelector.
     */
    constructor(small_cal: HTMLElement) {
        super();
        this.small_cal = small_cal;
        this.current_cell = null
    }

    update(now: Date) {
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

    timer_id: number

    constructor() {
        super();

        this.timer_id = 0
    }

    connectedCallback() {
        let interval = this.hasAttribute('interval')
            ? +(this.getAttribute('interval') as string)
            : 60;
        interval *= 1000 /* ms */

        this.timer_id = window.setInterval(() => this.update(new Date), interval)
        this.update(new Date)
    }

    static get observedAttributes() {
        return ['timer_id']
    }

    update(_: Date) { /* noop */ }
}


/**
 * Updates the ``Today'' link in the side panel to point directly to the
 * correct web-address. The link works without JavaScript, but then
 * requires a redirect from the server.
 *
 * All actual updating logic is already abstracted away. It would be
 * desirable if something more was done with this.
 */
class TodayButton extends ClockElement {
    a: HTMLAnchorElement;

    constructor() {
        super();
        this.a = document.createElement('a');
        this.a.textContent = 'Idag';
        this.a.classList.add('btn');
    }

    connectedCallback() {
        super.connectedCallback();
        this.replaceChildren(this.a);
    }

    update(now: Date) {
        this.a.href = now.format("~Y-~m-~d.html")
    }
}


class CurrentTime extends ClockElement {
    update(now: Date) {
        this.textContent = now.format('~H:~M:~S')
    }
}

function initialize_clock_components() {
    customElements.define('today-button', TodayButton)
    customElements.define('current-time', CurrentTime)
}
