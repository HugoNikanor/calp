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
  * TODO why isn't Timebar and SmallCellHighlight also Web Components?
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

    /** The bar to update */
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

    /** The calendar which a cell should be highlighted in */
    small_cal: HTMLElement
    /**
       The currently highlighted cell, or `null` if no cell should be
       should be highlighted (such as if a non-current month is selected
    */
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

/**
   Base class for custom HTML elements which wants to be updated for a human
   timescale.

   When creating, the attribute `interval` can be given, which specifies (in
   seconds) how often the component should be updated.
*/
class ClockElement extends HTMLElement {

    /** Javascript timer id. Used if the timer needs to be canceled */
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

    /**
       Method which is called each "tick" (see interval)
       @param date
         The current timestamp when the function is called.
     */
    update(_: Date) { /* noop */ }
}


/**
   A "button" which always points to the link "~Y-~m-~d.html".

   This class is bound to the web component <today-button />

   In the backend code, a `/today` endpoint exists. That however requires that
   we ask the server for the correct URL, and follow a 300 (series) redirect.

   Since the URL:s are stable, it's possible to jump directly to the given page.
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


/**
   A component which displays the current time

   This class is bound to the web component <current-time />

   It currently is hard-coded to display time on the format ~H:~M:~S.
*/
class CurrentTime extends ClockElement {
    update(now: Date) {
        this.textContent = now.format('~H:~M:~S')
    }
}

/**
   Create Web Components mentioned on this page.

   MUST be called early on in the execution.

   TODO this should be merged with other web component declarations.
*/
function initialize_clock_components() {
    customElements.define('today-button', TodayButton)
    customElements.define('current-time', CurrentTime)
}
