export { jcal_to_xcal }
import { ical_type, JCalProperty, JCal } from './types'
import { xcal, asList } from './lib'

function jcal_type_to_xcal(doc: Document, type: ical_type, value: any): Element {
    let el = doc.createElementNS(xcal, type);
    switch (type) {
        case 'boolean':
            el.textContent = value ? "true" : "false";
            break;

        case 'float':
        case 'integer':
            el.textContent = '' + value;
            break;

        case 'period':
            let [start, end] = value;
            let startEl = doc.createElementNS(xcal, 'start');
            startEl.textContent = start;
            let endEl: Element;
            if (end.find('P')) {
                endEl = doc.createElementNS(xcal, 'duration');
            } else {
                endEl = doc.createElementNS(xcal, 'end');
            }
            endEl.textContent = end;
            el.appendChild(startEl);
            el.appendChild(endEl);
            break;

        case 'recur':
            for (var key in value) {
                if (!value.hasOwnProperty(key)) continue;
                let e = doc.createElementNS(xcal, key);
                e.textContent = value[key];
                el.appendChild(e);
            }
            break;

        case 'date':
        // case 'time':
        case 'date-time':

        case 'duration':

        case 'binary':
        case 'text':
        case 'uri':
        case 'cal-address':
        case 'utc-offset':
            el.textContent = value;
            break;

        default:
        /* TODO error */
    }
    return el;
}

function jcal_property_to_xcal_property(
    doc: Document,
    jcal: JCalProperty
): Element {
    let [propertyName, params, type, ...values] = jcal;

    let tag = doc.createElementNS(xcal, propertyName);

    /* setup parameters */
    let paramEl = doc.createElementNS(xcal, 'params');
    for (var key in params) {
        /* Check if the key actually belongs to us.
           At least (my) format also appears when iterating
           over the parameters. Probably a case of builtins
           vs user defined.

           This is also the reason we can't check if params
           is empty beforehand, and instead check the
           number of children of paramEl below.
        */
        if (!params.hasOwnProperty(key)) continue;

        let el = doc.createElementNS(xcal, key);

        for (let v of asList(params.get(key))) {
            let text = doc.createElementNS(xcal, 'text');
            text.textContent = '' + v;
            el.appendChild(text);
        }

        paramEl.appendChild(el);
    }

    if (paramEl.childElementCount > 0) {
        tag.appendChild(paramEl);
    }

    /* setup value (and type) */
    // let typeEl = doc.createElementNS(xcal, type);

    switch (propertyName) {
        case 'geo':
            if (type == 'float') {
                // assert values[0] == [x, y]
                let [x, y] = values[0];
                let lat = doc.createElementNS(xcal, 'latitude')
                let lon = doc.createElementNS(xcal, 'longitude')
                lat.textContent = x;
                lon.textContent = y;
                tag.appendChild(lat);
                tag.appendChild(lon);
            } else {
                /* TODO, error */
            }
            break;
        /* TODO reenable this
    case 'request-status':
        if (type == 'text') {
            // assert values[0] instanceof Array
            let [code, desc, ...data] = values[0];
            let codeEl = doc.createElementNS(xcal, 'code')
            code.textContent = code;
            tag.appendChild(codeEl);


            let descEl = doc.createElementNS(xcal, 'description')
            desc.textContent = desc;
            tag.appendChild(descEl);

            if (data !== []) {
                data = data[0];
                let dataEl = doc.createElementNS(xcal, 'data')
                data.textContent = data;
                tag.appendChild(dataEl);
            }
        } else {
            /* TODO, error * /
        }
        break;
        */
        default:
            for (let value of values) {
                tag.appendChild(jcal_type_to_xcal(doc, type, value))
            }
    }

    return tag;
}


function jcal_to_xcal(...jcals: JCal[]): Document {
    let doc = document.implementation.createDocument(xcal, 'icalendar');
    for (let jcal of jcals) {
        doc.documentElement.appendChild(jcal_to_xcal_inner(doc, jcal));
    }
    return doc;
}

function jcal_to_xcal_inner(doc: Document, jcal: JCal) {
    let [tagname, properties, components] = jcal;

    let xcal_tag = doc.createElementNS(xcal, tagname);

    /* I'm not sure if the properties and components tag should be left out
       when empty. It should however NOT be an error to leave them in.
    */

    let xcal_properties = doc.createElementNS(xcal, 'properties');
    for (let property of properties) {
        xcal_properties.appendChild(jcal_property_to_xcal_property(doc, property));
    }

    let xcal_children = doc.createElementNS(xcal, 'components');
    for (let child of components) {
        xcal_children.appendChild(jcal_to_xcal_inner(doc, child));
    }

    xcal_tag.appendChild(xcal_properties);
    xcal_tag.appendChild(xcal_children);

    return xcal_tag;

}
