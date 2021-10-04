function recur_xml_to_rrule(dom_element) {
    let rr = new RRule;
    for (let child of dom_element.children) {
        let key = child.tagName; /* freq */
        let val = child.textContent; /* weekly */
        rr[key] = val;
    }
    return rr;
}

function recur_jcal_to_rrule(jcal) {
    let rr = new RRule;
    for (var key in jcal) {
        rr[key] = jcal[key];
    }
    return rr;
}

class RRule {

    /* direct access to fields is fine */
    /* setting them however requires methods, since there might
       be listeners */

    fields = ['freq', 'until', 'count', 'interval',
              'bysecond', 'byminute', 'byhour',
              'bymonthday', 'byyearday', 'byweekno',
              'bymonth', 'bysetpos', 'wkst',
              'byday'
             ]

    constructor() {

        this.listeners = {}

        for (let f of this.fields) {
            this[f] = false;
            Object.defineProperty(
                this, f, {
                    /*
                      TODO many of the fields should be wrapped
                      in type tags. e.g. <until> elements are either
                      <date> or <date-time>, NOT a raw date.
                      by* fields should be wrapped with multiple values.
                     */
                    get: () => this['_' + f],
                    set: (v) => {
                        this['_' + f] = v
                        for (let l of this.listeners[f]) {
                            l(v);
                        }
                    }
                });
            this.listeners[f] = [];
        }
    }

    addListener(field, proc) {
        this.listeners[field].push(proc);
    }

    /* NOTE this function is probably never used.
       Deperate it and refer to RRule.asJcal 
       together with jcal_to_xcal */
    asXcal(doc) {
        /* TODO empty case */
        // let str = "<recur>";
        let root = doc.createElementNS(xcal, 'recur');
        for (let f of this.fields) {
            let v = this.fields[f];
            if (! v) continue;
            let tag = doc.createElementNS(xcal, f);
            /* TODO type formatting */
            tag.textContent = `${v}`;
            root.appendChild(tag);
        }
        return root;
    }

    asJcal() {
        let obj = {};
        for (let f of this.fields) {
            let v = this[f];
            if (! v) continue;
            /* TODO special formatting for some types */
            obj[f] = v;
        }
        return obj;
    }

    /*
    asIcal() {
        return this.fields
            .map(f => [f, this.fields[f]])
            .filter([_, v] => v)
            .map(([k, v]) => `${k}=${v}`)
            .join(';');
    }
    */
};
