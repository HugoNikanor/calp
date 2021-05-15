/*
  bind (event_component, field_to_bind)
*/

/* vcalendar element */

function bind_recur(el, e) {
    /* todo bind default slots of rrule */

    let p = el.properties.get_callback_list('rrule');
    // let rrule = el.rrule;

    /* add listeners to bind-rr tags */
    for (let rr of e.querySelectorAll('.bind-rr')) {
		/* TODO handle byday */
        if (rr.classList.contains('input-list')) {
            rr.addEventListener('input', function () {
                let name = rr.attributes.name.value;
                el.properties.rrule[name] = this.get_value();
            });
        } else if (rr.tagName === 'input' || rr.classList.contains('date-time')) {
            rr.addEventListener('input', function () {
                console.log(this);
                el.properties.rrule[rr.name] = this.value;
            });
        } else if (rr.tagName === 'select') {
            rr.addEventListener('change', function () {
                let opt = this.options[this.selectedIndex];
                let v = opt.value;
                // console.log(v);
                el.properties.rrule[rr.name] = v;
            });
        }
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
                console.log("Implement me!");
                break;
            default:
                if (input_field.classList.contains('date-time')) {
                    let date = input_field.querySelector('input[type=date]');
                    let time = input_field.querySelector('input[type=time]');
                } else if (input_field.classList.contains('input-list')) {
                } else {
                    console.log(input_field);
                    throw Error();
                }
            }
        }
    }]);


}

function bind_edit(el, e) {
    let p = el.properties.get_callback_list(e.dataset.property);
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
    el.properties.get_callback_list(e.dataset.property).push([e, f]);
}


function bind_wholeday(el, e) {
    let popup = popup_from_event(el);
    let wholeday = popup.querySelector("input[name='wholeday']");
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
