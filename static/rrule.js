function recur_xml_to_rrule(dom_element) {
    let rr = new RRule;
    for (let child of dom_element.children) {
        let key = child.tagName; /* freq */
        let val = child.innerHTML; /* weekly */
        rr[key] = val;
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
              'bymonth', 'bysetpos', 'wkst']

    /*
      TODO multi valued byhour should be represented as
      <byhour>1</byhour><byhour>2</byhour>
      NOT as <byhour>1,2</byhour> as it currently does.
     */

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

    asXcal() {
        /* TODO empty case */
        let str = "<recur>";
        for (let f of this.fields) {
            let v = this.fields[f];
            if (! v) continue;
            str += `<${f}>${v}</${f}>`;
        }
        str += "</recur>";
        return str;
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
