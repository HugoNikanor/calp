export { ComponentVEvent }

import { vcal_objects } from '../globals'
import { VEvent } from '../vevent'

/* Root component for all events which content is closely linked to a
@code{VEvent} object

Lacks an accompaning tag, and shouldn't be directly instanciated.
*/
abstract class ComponentVEvent extends HTMLElement {

    template: HTMLTemplateElement
    uid: string

    constructor(uid?: string) {
        super();
        this.template = document.getElementById(this.tagName) as HTMLTemplateElement;

        let real_uid;
        if (this.dataset.uid) uid = this.dataset.uid;
        if (uid) real_uid = uid;

        if (!real_uid) {
            console.warn(this.outerHTML);
            throw `UID required`
        }

        this.uid = real_uid;
        this.dataset.uid = uid;

        vcal_objects.get(this.uid)?.register(this);

        /* We DON'T have a redraw here in the general case, since the
           HTML rendered server-side should be fine enough for us.
           Those that need a direct rerendering (such as the edit tabs)
           should take care of that some other way */
    }

    connectedCallback() {
        let uid = this.dataset.uid
        if (uid) {
            let v = vcal_objects.get(uid)
            if (v) this.redraw(v);
        }
    }

    abstract redraw(data: VEvent): void

}
