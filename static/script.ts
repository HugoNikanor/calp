import { VEvent, xml_to_vcal } from './vevent'
import { SmallcalCellHighlight, Timebar } from './clock'
import { vcal_objects, event_calendar_mapping } from './globals'
import { EventCreator } from './event-creator'
import { PopupElement, setup_popup_element } from './components/popup-element'
import { initialize_components } from './elements'

/*
  calp specific stuff
*/

window.addEventListener('load', function() {

    /*
      TODO possibly check here that both window.EDIT_MODE and window.VIEW have
      defined values.
     */

    // let json_objects_el = document.getElementById('json-objects');
    let div = document.getElementById('xcal-data')!;
    let vevents = div.firstElementChild!.children;

    for (let vevent of vevents) {
        let ev = xml_to_vcal(vevent);
        vcal_objects.set(ev.getProperty('uid'), ev)
    }


    let div2 = document.getElementById('calendar-event-mapping')!;
    for (let calendar of div2.children) {
        let calendar_name = calendar.getAttribute('key')!;
        for (let child of calendar.children) {
            let uid = child.textContent;
            if (!uid) {
                throw "UID required"
            }
            event_calendar_mapping.set(uid, calendar_name);
            let obj = vcal_objects.get(uid);
            if (obj) obj.calendar = calendar_name
        }
    }

    initialize_components();

    /* A full redraw here is WAY to slow */
    // for (let [_, obj] of vcal_objects) {
    //     for (let registered of obj.registered) {
    //         registered.redraw(obj);
    //     }
    // }



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
    if (true && window.EDIT_MODE) {
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
                    let uid = ev.getProperty('uid');
                    vcal_objects.set(uid, ev);
                    setup_popup_element(ev);
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
                    let uid = ev.getProperty('uid');
                    vcal_objects.set(uid, ev);
                    setup_popup_element(ev);
                });
        }
    }

    for (let el of document.getElementsByClassName("event")) {
        /* Popup script replaces need for anchors to events.
           On mobile they also have the problem that they make
           the whole page scroll there.
        */
        el.parentElement!.removeAttribute("href");
    }

    document.onkeydown = function(evt) {
        evt = evt || window.event;
        if (!evt.key) return;
        if (evt.key.startsWith("Esc")) {
            for (let popup of document.querySelectorAll("popup-element[visible]")) {
                popup.removeAttribute('visible')
            }
        }
    }

    /* ---------------------------------------- */

    document.addEventListener('keydown', function(event) {
        /* Physical key position, names are what that key would
           be in QWERTY */
        let i = ({
            'KeyQ': 0,
            'KeyW': 1,
            'KeyE': 2,
            'KeyR': 3,
            'KeyT': 4,
            'KeyY': 5,
        })[event.code];
        if (i === undefined) return
        if (!PopupElement.activePopup) return;
        let element = PopupElement
            .activePopup
            .querySelectorAll("[role=tab]")[i] as HTMLInputElement | undefined
        if (!element) return;
        /* don't switch tab if event was fired while writing */
        if ('value' in (event.target as any)) return;
        element.click();
    });

    document.addEventListener('keydown', function(event) {
        if (event.key !== '/') return;
        if ('value' in (event.target as any)) return;

        let searchbox = document.querySelector('.simplesearch [name=q]') as HTMLInputElement
        // focuses the input, and selects all the text in it
        searchbox.select();
        event.preventDefault();
    });
})
