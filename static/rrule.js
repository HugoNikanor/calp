class RRule {

    /* direct access to fields is fine */
    /* setting them however requires methods, since there might
       be listeners */

    const fields = ['freq', 'until', 'count', 'interval',
                    'bysecond', 'byminute', 'byhour',
                    'bymonthday', 'byyearday', 'byweekno',
                    'bymonth', 'bysetpos', 'wkst']

    constructor() {

        this.listeners = {}

        for (let f of this.fields) {
            this[f] = false;
            Object.defineProperty(
                this, f, {
                    get: () => this['_' + f];
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
        this.listeners[field].append(proc);
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
