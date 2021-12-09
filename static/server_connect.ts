export { create_event, remove_event }

import { jcal_to_xcal } from './jcal'
import { VEvent } from './vevent'
import { uid } from './types'
import { vcal_objects } from './globals'

async function remove_event(uid: uid) {
    let element = vcal_objects.get(uid);
    if (!element) {
        console.error(`No VEvent with that uid = '${uid}', giving up`)
        return;
    }

    let data = new URLSearchParams();
    data.append('uid', uid);

    let response = await fetch('/remove', {
        method: 'POST',
        body: data
    });

    console.log(response);
    // toggle_popup(popup_from_event(element));

    if (response.status < 200 || response.status >= 300) {
        let body = await response.text();
        alert(`HTTP error ${response.status}\n${body}`)
    } else {
        /* Remove all HTML components which belong to this vevent */
        for (let component of element.registered) {
            component.remove();
        }
        /* remove the vevent from our global store,
           hopefully also freeing it for garbace collection */
        vcal_objects.delete(uid);
    }
}

// function event_to_jcal(event) {
//     /* encapsulate event in a shim calendar, to ensure that
//        we always send correct stuff */
//     return ['vcalendar',
//         [
//             /*
//               'prodid' and 'version' are technically both required (RFC 5545,
//               3.6 Calendar Components).
//             */
//         ],
//         [
//             /* vtimezone goes here */
//             event.properties.to_jcal()
//         ]
//     ];
// }

async function create_event(event: VEvent) {

    // let xml = event.getElementsByTagName("icalendar")[0].outerHTML
    let calendar = event.calendar;
    if (!calendar) {
        console.error("Can't create event without calendar")
        return;
    }

    console.log('calendar=', calendar/*, xml*/);

    let data = new URLSearchParams();
    data.append("cal", calendar);
    // data.append("data", xml);

    // console.log(event);

    let jcal = event.to_jcal();
    // console.log(jcal);

    let doc: Document = jcal_to_xcal(jcal);
    // console.log(doc);
    let str = doc.documentElement.outerHTML;
    console.log(str);
    data.append("data", str);

    // console.log(event.properties);

    let response = await fetch('/insert', {
        method: 'POST',
        body: data
    });

    console.log(response);
    if (response.status < 200 || response.status >= 300) {
        let body = await response.text();
        alert(`HTTP error ${response.status}\n${body}`)
        return;
    }

    /* response from here on is good */

    // let body = await response.text();

    /* server is assumed to return an XML document on the form
       <properties>
       **xcal property** ...
       </properties>
       parse that, and update our own vevent with the data.
    */

    // let parser = new DOMParser();
    // let return_properties = parser
    //     .parseFromString(body, 'text/xml')
    //     .children[0];

    // let child;
    // while ((child = return_properties.firstChild)) {
    //     let target = event.querySelector(
    //         "vevent properties " + child.tagName);
    //     if (target) {
    //         target.replaceWith(child);
    //     } else {
    //         event.querySelector("vevent properties")
    //             .appendChild(child);
    //     }
    // }

    for (let r of event.registered) {
        r.classList.remove('generated');
        if (r.tagName.toLowerCase() === 'popup-element') {
            console.log(r);
            r.removeAttribute('visible');
        }
    }
}
