import { close_all_popups } from './popup'
import { VEvent, xml_to_vcal } from './vevent'
import { SmallcalCellHighlight, Timebar } from './clock'
import { makeElement } from './lib'
import { vcal_objects, event_calendar_mapping, EDIT_MODE } from './globals'
import { open_popup } from './popup'
import { EventCreator } from './event-creator'
import { PopupElement } from './components/popup-element'
import { initialize_components } from './elements'

/*
  calp specific stuff
*/

window.addEventListener('load', function() {

    // let json_objects_el = document.getElementById('json-objects');
    let div = document.getElementById('xcal-data')!;
    let vevents = div.firstElementChild!.children;

    for (let vevent of vevents) {
        let ev = xml_to_vcal(vevent);
        vcal_objects.set(ev.getProperty('uid'), ev)
    }


    let div2 = document.getElementById('calendar-event-mapping')!;
    for (let calendar of div2.children) {
        for (let child of calendar.children) {
            event_calendar_mapping.set(
                child.innerHTML, calendar.getAttribute('key')!);
        }
    }

    /*
      - .popup
      - .block
      - .list
     */
    /*
    let vevent_els = document.getElementsByClassName('vevent')
    for (let el of vevent_els) {
        try {
            vcal_objects[el.dataset.uid].register(el);
        } catch {
            console.error("Invalid something, uid = ", el.dataset.uid,
                          "el = ", el
                         );
        }
    }
    */

    initialize_components();




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
