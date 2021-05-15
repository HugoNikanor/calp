
async function remove_event (element) {
    let uid = element.querySelector("icalendar uid text").innerHTML;

    let data = new URLSearchParams();
    data.append('uid', uid);

    let response = await fetch ( '/remove', {
        method: 'POST',
        body: data
    });

    console.log(response);
    toggle_popup("popup" + element.id);

    if (response.status < 200 || response.status >= 300) {
        let body = await response.text();
        alert(`HTTP error ${response.status}\n${body}`)
    } else {
        element.remove();
    }
}

function event_to_jcal (event) {
    /* encapsulate event in a shim calendar, to ensure that
       we always send correct stuff */
    return ['vcalendar',
            [
                /*
                  'prodid' and 'version' are technically both required (RFC 5545,
                  3.6 Calendar Components).
                */
            ],
            [
                /* vtimezone goes here */
                event.properties.to_jcal()
            ]
           ];
}

async function create_event (event) {

    // let xml = event.getElementsByTagName("icalendar")[0].outerHTML
    let calendar = event.properties.calendar;

    console.log(calendar/*, xml*/);

    let data = new URLSearchParams();
    data.append("cal", calendar);
    // data.append("data", xml);

    console.log(event);

    let jcal = event_to_jcal(event);



    let doc = jcal_to_xcal(jcal);
    console.log(doc);
    let str = doc.documentElement.outerHTML;
    console.log(str);
    data.append("data", str);

    // console.log(event.properties);

    // return;

    let response = await fetch ( '/insert', {
        method: 'POST',
        body: data
    });

    console.log(response);
    if (response.status < 200 || response.status >= 300) {
        let body = await response.text();
        alert(`HTTP error ${response.status}\n${body}`)
        return;
    }

    let body = await response.text();

    /* server is assumed to return an XML document on the form
       <properties>
       **xcal property** ...
       </properties>
       parse that, and update our own vevent with the data.
    */

    let parser = new DOMParser();
    let return_properties = parser
        .parseFromString(body, 'text/xml')
        .children[0];

    let child;
    while ((child = return_properties.firstChild)) {
        let target = event.querySelector(
            "vevent properties " + child.tagName);
        if (target) {
            target.replaceWith(child);
        } else {
            event.querySelector("vevent properties")
                .appendChild(child);
        }
    }

    event.classList.remove("generated");
    toggle_popup("popup" + event.id);
}
