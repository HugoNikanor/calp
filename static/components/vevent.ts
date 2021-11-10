export { ComponentVEvent }

import { vcal_objects } from '../globals'
import { VEvent } from '../vevent'

/* Root component for all events which content is closely linked to a
@code{VEvent} object

Lacks an accompaning tag, and shouldn't be directly instanciated.
*/
class ComponentVEvent extends HTMLElement {

    template: HTMLTemplateElement
    uid: string

    constructor(uid?: string) {
        super();
        this.template = document.getElementById(this.tagName) as HTMLTemplateElement;

        let real_uid;
        if (this.dataset.uid) uid = this.dataset.uid;
        if (uid) real_uid = uid;

        if (!real_uid) {
            throw `UID required`
        }

        this.uid = real_uid;

        vcal_objects.get(this.uid)?.register(this);

        /* We DON'T have a redraw here in the general case, since the
           HTML rendered server-side should be fine enough for us.
           Those that need a direct rerendering (such as the edit tabs)
           should take care of that some other way */
    }

    connectedCallback() {
        let uid, v;
        if ((uid = this.dataset.uid)) {
            v = vcal_objects.get(uid)
            if (v) this.redraw(v);
        }
    }

    redraw(data: VEvent) {
        // update ourselves from template

        if (!this.template) {
            throw "Something";
        }

        let body = (this.template.content.cloneNode(true) as DocumentFragment).firstElementChild!;

        for (let el of body.getElementsByClassName("bind")) {
            if (!(el instanceof HTMLElement)) continue;
            let p = el.dataset.property!;
            let d, fmt;
            if ((d = data.getProperty(p))) {
                if ((fmt = el.dataset.fmt)) {
                    el.innerHTML = d.format(fmt);
                } else {
                    el.innerHTML = d;
                }
            }
        }

        this.replaceChildren(body);
    }

}
