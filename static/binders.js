
/* vcalendar element */

function bind_recur(el, e) {
    /* todo bind default slots of rrule */

    let p = get_property(el, 'rrule', new rrule);
    let rrule = el.rrule;

    for (let rr of e.querySelectorAll('.bind-rr')) {
        rrule.addListener(rr.dataset.name, v => {
            /* TODO Different depending on tag type */
            /* TODO scoope of rr? */
            if (! v) {
                rr.value = '';
            } else {
                rr.vaule = v;
            }
        });
    }

    p.push([e, function (s, v) {
        /* v is an rrule object */
        for (let f of v.fields) {
            let input_field = s.querySelector(`[name=${f}]`);
            switch (input_field.tagName) {
            case 'input':
                input_field.value = v;
                break;
            case 'select':
                /* TODO */
                break;
            default:
                if (input_field.classList.contains('date-time')) {
                    let date = input_field.querySelector('input[type=date]');
                    let time = input_field.querySelector('input[type=time]');
                } else if (e.classList.contains('input-list')) {
                } else {
                    throw Error();
                }
            }
        }
    }]);


}

function bind_edit(el, e) {
    let p = get_property(el, e.dataset.property);
    e.addEventListener('input', function () {
        el.properties[e.dataset.property] = this.value;
    });
    let f;
    switch (e.tagName) {
    case 'input':
        switch (e.type) {
        case 'time': f = (s, v) => s.value = v.format("~H:~M"); break;
        case 'date': f = (s, v) => s.value = v.format("~Y-~m-~d"); break;
            // TODO remaining types cases
        default: f = (s, v) => s.value = v;
        }
        p.push([e, f])
        break;
    case 'textarea':
        f = (s, v) => s.innerHTML = v;
        p.push([e, f])
        break;
    default:
        alert("How did you get here??? " + e.tagName)
        break;
    }

}

function bind_view(el, e) {
        let f = (s, v) => s.innerHTML = v.format(s.dataset && s.dataset.fmt);
        get_property(el, e.dataset.property).push([e, f]);
}


function bind_wholeday(el, e) {
    // let wholeday = popup.querySelector("input[name='wholeday']");
    let popup = popup_from_event(el);
    wholeday.addEventListener('click', function (event) {
        for (let f of popup.querySelectorAll("input[type='time']")) {
            f.disabled = wholeday.checked;
        }

        for (let f of ['dtstart', 'dtend']) {
            let d = el.properties[f];
            if (! d) continue; /* dtend optional */
            d.isWholeDay = wholeday.checked;
            el.properties[f] = d;
        }
    });
}
