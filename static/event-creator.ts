export { EventCreator }

import { VEvent } from './vevent'
import { v4 as uuid } from 'uuid'
import { ComponentBlock } from './components/vevent-block'
import { round_time } from './lib'
import { parseDate } from './lib'

class EventCreator {

    /* Event which we are trying to create */
    ev: VEvent | null = null;

    /* Graphical block for event. Only here so we can find its siblings,
       and update pointer events accordingly */
    event: Element | null = null;

    event_start: { x: number, y: number } = { x: NaN, y: NaN }
    down_on_event: boolean = false
    timeStart: number = 0

    create_event_down(intended_target: HTMLElement): (e: MouseEvent) => any {
        let that = this;
        return function(e: MouseEvent) {
            /* Only trigger event creation stuff on actuall events background,
               NOT on its children */
            that.down_on_event = false;
            if (e.target != intended_target) return;
            that.down_on_event = true;

            that.event_start.x = e.clientX;
            that.event_start.y = e.clientY;
        }
    }

    /*
      round_to: what start and end times should round to when dragging, in fractionsb
      of the width of the containing container.

      TODO limit this to only continue when on the intended event_container.

      (event → [0, 1)), 𝐑, bool → event → ()
     */
    create_event_move(
        pos_in: ((c: HTMLElement, e: MouseEvent) => number),
        round_to: number = 1,
        wide_element: boolean = false
    ): ((e: MouseEvent) => any) {
        let that = this;
        return function(this: HTMLElement, e: MouseEvent) {
            if (e.buttons != 1 || !that.down_on_event) return;

            /* Create event when we start moving the mouse. */
            if (!that.ev) {
                /* Small deadzone so tiny click and drags aren't registered */
                if (Math.abs(that.event_start.x - e.clientX) < 10
                    && Math.abs(that.event_start.y - e.clientY) < 5) { return; }

                /* only allow start of dragging on background */
                if (e.target !== this) return;

                /* only on left click */
                if (e.buttons != 1) return;

                // let [popup, event] = that.create_empty_event();
                // that.event = event;
                that.ev = new VEvent();
                that.ev.setProperty('summary', 'Created Event');
                that.ev.setProperty('uid', uuid())

                // let ev_block = document.createElement('vevent-block') as ComponentBlock;
                let ev_block = new ComponentBlock(that.ev.getProperty('uid'));
                that.event = ev_block;
                that.ev.register(ev_block);

                /* TODO better solution to add popup to DOM */
                // document.getElementsByTagName("main")[0].append(popup);

                /* [0, 1) -- where are we in the container */
                /* Ronud to force steps of quarters */
                /* NOTE for in-day events a floor here work better, while for
                   all day events I want a round, but which has the tip over point
                   around 0.7 instead of 0.5.
                   It might also be an idea to subtract a tiny bit from the short events
                   mouse position, since I feel I always get to late starts.
                */

                // that.event.dataset.time1 = '' + time;
                // that.event.dataset.time2 = '' + time;

                /* ---------------------------------------- */

                this.appendChild(ev_block);

                /* requires that event is child of an '.event-container'. */
                // new VComponent(
                //     event,
                //     wide_element=wide_element);
                // bind_properties(event, wide_element);

                /* requires that dtstart and dtend properties are initialized */

                /* ---------------------------------------- */

                /* Makes all current events transparent when dragging over them.
                   Without this weird stuff happens when moving over them

                   This includes ourselves.
                */
                for (let e of this.children) {
                    (e as HTMLElement).style.pointerEvents = "none";
                }

                that.timeStart = round_time(pos_in(this, e), round_to);
            }

            let time = round_time(pos_in(this, e), round_to);

            // let time1 = Number(that.event.dataset.time1);
            // let time2 = round_time(
            //     pos_in(that.event.parentElement!, e),
            //     round_to);
            // that.event.dataset.time2 = '' + time2

            /* ---------------------------------------- */

            let event_container = this.closest(".event-container") as HTMLElement;

            /* These two are in UTC */
            let container_start = parseDate(event_container.dataset.start!);
            let container_end = parseDate(event_container.dataset.end!);

            /* ---------------------------------------- */

            /* ms */
            let duration = container_end.valueOf() - container_start.valueOf();

            let start_in_duration = duration * Math.min(that.timeStart, time);
            let end_in_duration = duration * Math.max(that.timeStart, time);

            /* Notice that these are converted to UTC, since the intervals are given
               in utc, and I only really care about local time (which a specific local
               timezone doesn't give me)
            */
            /* TODO Should these inherit UTC from container_*? */
            let d1 = new Date(container_start.getTime() + start_in_duration)
            let d2 = new Date(container_start.getTime() + end_in_duration)

            /* TODO these writes should preferably be grouped,
               to save a redraw for all registered listeners */
            that.ev.setProperty('dtstart', d1);
            that.ev.setProperty('dtend', d2);

            // console.log(that.event);
            // console.log(d1.format("~L~H:~M"), d2.format("~L~H:~M"));
        }
    }

    create_event_finisher(callback: ((ev: VEvent) => void)) {
        let that = this;
        return function create_event_up(e: MouseEvent) {
            if (!that.ev) return;

            /* Restore pointer events for all existing events.
               Allow pointer events on our new event
            */
            for (let e of (that.event as Element).parentElement!.children) {
                (e as HTMLElement).style.pointerEvents = "";
            }

            let localevent = that.ev;
            that.ev = null
            that.event = null;

            callback(localevent);

        }
    }
}
