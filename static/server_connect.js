
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

async function create_event (event) {

    // let xml = event.getElementsByTagName("icalendar")[0].outerHTML
    let calendar = event.properties.calendar;

    console.log(calendar/*, xml*/);

    let data = new URLSearchParams();
    data.append("cal", calendar);
    // data.append("data", xml);

    for (let prop of event.properties.ical_properties) {
        let v = event.properties[prop];
        if (v !== undefined) {
            [prop, {}, /*type*/, v];
            /* TODO , here */

            let type = 'text';
            let value;

            if (v instanceof Array) {
            } else if (v instanceof Date) {
                if (v.isWholeDay) {
                    type = 'date';
                    value = v.format("~Y-~m-~d");
                } else {
                    type = 'date-time';
                    /* TODO TZ */
                    value = v.format("~Y-~m~dT~H:~M:~S");
                }
            } else if (v === true || v === false) {
                type = 'boolean';
                value = v;
            } else if (typeof(v) == 'number') {
                /* TODO float or integer */
                type = 'integer';
                value = v;
            }
            /* TODO period */
            /* TODO recur */
            else {
                /* text types */
            }

        }
    }


    let jcal =
        ['vcalendar',
         ['vevent',
          [
              ['summary', {}, 'text', 'Example summary'],
          ],
          []
         ]
        ];

    let doc = jcal_to_xcal(jcal);
    console.log(doc);

    console.log(event.properties);

    return;

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
    let properties = parser
        .parseFromString(body, 'text/xml')
        .children[0];

    let child;
    while ((child = properties.firstChild)) {
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
