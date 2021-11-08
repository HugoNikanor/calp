import { close_all_popups } from './popup'
import { VEvent } from './vevent'
import { SmallcalCellHighlight, Timebar } from './clock'
import { makeElement, parseDate, round_time } from './lib'
import { vcal_objects, ComponentBlock, PopupElement } from './globals'
import { open_popup } from './popup'

import { v4 as uuid } from 'uuid'

/*
  calp specific stuff
*/

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

declare let EDIT_MODE: boolean

window.addEventListener('load', function() {
    // let start_time = document.querySelector("meta[name='start-time']").content;
    // let end_time = document.querySelector("meta[name='end-time']").content;

    const sch = new SmallcalCellHighlight(
        document.querySelector('.small-calendar')!)

    const timebar = new Timebar(/*start_time, end_time*/);

    timebar.update(new Date);
    sch.update(new Date);
    window.setInterval(() => {
        let d = new Date;
        timebar.update(d);
        sch.update(d);
    }, 1000 * 60);

    /* Is event creation active? */
    if (true && EDIT_MODE) {
        let eventCreator = new EventCreator;
        for (let c of document.getElementsByClassName("events")) {
            if (!(c instanceof HTMLElement)) continue;
            c.addEventListener('mousedown', eventCreator.create_event_down(c));
            c.addEventListener('mousemove', eventCreator.create_event_move(
                (c, e) => e.offsetY / c.clientHeight,
                /* every quarter, every hour */
                1 / (24 * 4), false
            ));
            c.addEventListener('mouseup', eventCreator.create_event_finisher(
                function(ev: VEvent) {
                    // let popup = document.createElement('popup-element') as PopupElement;

                    let uid = ev.getProperty('uid');

                    vcal_objects.set(uid, ev);

                    let popup = new PopupElement(uid);
                    /* TODO these things fail, due to the event not being
                    present in the global_events map */
                    (document.querySelector('.days') as Element).appendChild(popup);
                    ev.register(popup);
                    open_popup(popup);
                    console.log(popup);
                    // (popup.querySelector("input[name='summary']") as HTMLInputElement).focus();
                    // let popupElement = document.getElementById("popup" + event.id);
                    // open_popup(popup_from_event(event));

                    // popupElement.querySelector("input[name='summary']").focus();

                }));
        }

        for (let c of document.getElementsByClassName("longevents")) {
            if (!(c instanceof HTMLElement)) continue;
            c.onmousedown = eventCreator.create_event_down(c);
            c.onmousemove = eventCreator.create_event_move(
                (c, e) => e.offsetX / c.clientWidth,
                /* every day, NOTE should be changed to check
                   interval of longevents */
                1 / 7, true
            );
            c.onmouseup = eventCreator.create_event_finisher(
                function(ev: VEvent) {
                    // let popup = document.createElement('popup-element') as PopupElement;

                    let uid = ev.getProperty('uid');

                    vcal_objects.set(uid, ev);

                    let popup = new PopupElement(uid);
                    /* TODO these things fail, due to the event not being
                    present in the global_events map */
                    (document.querySelector('.days') as Element).appendChild(popup);
                    ev.register(popup);
                    open_popup(popup);
                    console.log(popup);
                    // (popup.querySelector("input[name='summary']") as HTMLInputElement).focus();
                    // let popupElement = document.getElementById("popup" + event.id);
                    // open_popup(popup_from_event(event));

                    // popupElement.querySelector("input[name='summary']").focus();


                    // ----------------------------------------------------------------------------------------------------
                    // TODO restore this
                    // let popupElement = document.getElementById("popup" + event.id);
                    // open_popup(popupElement);

                    // popupElement.querySelector("input[name='summary']").focus();

                    // /* This assumes that it's unchecked beforehand.
                    //    Preferably we would just ensure that it's checked here,
                    //    But we also need to make sure that the proper handlers
                    //    are run then */
                    // popupElement.querySelector("input[name='wholeday']").click();

                });
        }
    }

    // for (let nav of document.getElementsByClassName("popup-control")) {
    //     bind_popup_control(nav);
    // }

    for (let el of document.getElementsByClassName("event")) {
        /* Popup script replaces need for anchors to events.
           On mobile they also have the problem that they make
           the whole page scroll there.
        */
        el.parentElement!.removeAttribute("href");

        let popup = document.getElementById("popup" + el.id);
        // popup.getElementsByClassName("edit-form")[0].onsubmit = function () {
        //     create_event(el);
        //     return false; /* stop default */
        // }

        /* Bind all vcomponent properties into javascript. */
        // if (el.closest(".longevents")) {
        //     new VComponent(el, true);
        // } else {
        //     new VComponent(el, false);
        // }

    }

    document.onkeydown = function(evt) {
        evt = evt || window.event;
        if (!evt.key) return;
        if (evt.key.startsWith("Esc")) {
            close_all_popups();
        }
    }


    /* Replace backend-driven [today] link with frontend, with one that
       gets correctly set in the frontend. Similarly, update the go to
       specific date button into a link which updates wheneven the date
       form updates.
    */

    let gotodatebtn = document.querySelector("#jump-to .btn")!;
    let target_href = (new Date).format("~Y-~m-~d") + ".html";
    let golink = makeElement('a', {
        className: 'btn',
        href: target_href,
        innerHTML: gotodatebtn.innerHTML,
    }) as HTMLAnchorElement
    gotodatebtn.replaceWith(golink);

    (document.querySelector("#jump-to input[name='date']") as HTMLInputElement)
        .onchange = function() {
            let date = (this as HTMLInputElement).valueAsDate!.format("~Y-~m-~d");
            console.log(date);
            golink.href = date + ".html";
        }

    /* ---------------------------------------- */

    /* needs to be called AFTER bind_properties, but BEFORE init_input_list
       After bind_properties since that initializes categories to a possible field
       Before init_input_list since we need this listener to be propagated to clones.
       [CATEGORIES_BIND]
    */
    // TODO fix this
    // for (let lst of document.querySelectorAll(".input-list[data-property='categories']")) {
    //     let f = function() {
    //         console.log(lst, lst.closest('.popup-container'));
    //         let event = event_from_popup(lst.closest('.popup-container'))
    //         event.properties.categories = lst.get_value();
    //     };

    //     for (let inp of lst.querySelectorAll('input')) {
    //         inp.addEventListener('input', f);
    //     }
    // }

    // init_arbitary_kv();

    // init_input_list();


    document.addEventListener('keydown', function(event) {
        if (event.key == '/') {
            let searchbox = document.querySelector('.simplesearch [name=q]') as HTMLInputElement
            // focuses the input, and selects all the text in it
            searchbox.select();
            event.preventDefault();
        }
    });
})
