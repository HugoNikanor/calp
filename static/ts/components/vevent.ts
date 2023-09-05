/**
 * Root component for all events which content is closely linked to a `VEvent` object
 *
 * Lacks an accompaning tag, and shouldn't be directly instanciated.
 *
 * Note that many of these assume that their initial children are
 * configured specifically, that is however not completely documented.
 *
 * @category Web Components
 * @mergeTarget components
 * @module
 */

export { ComponentVEvent }

import { vcal_objects } from '../globals'
import { VEvent } from '../vevent'

abstract class ComponentVEvent extends HTMLElement {

    template?: HTMLTemplateElement
    uid: string

    /**
     * This registeres itself, but doesn't redraw
     * We do however redraw in connectedCallback

     * @privateRemarks
     * TODO what is done in the default constructor,
     * and the default connectedCallback
    */
    constructor(uid?: string) {
        super();
        this.template = document.getElementById(this.tagName.toLowerCase()) as HTMLTemplateElement | undefined

        let real_uid;

        if (uid) {
            // console.log('Got UID directly');
            real_uid = uid;
        } else {
            /* I know that this case is redundant, it's here if we don't want to
               look up the tree later */
            if (this.dataset.uid) {
                // console.log('Had UID as direct attribute');
                real_uid = this.dataset.uid;
            } else {
                let el = this.closest('[data-uid]')
                if (el) {
                    // console.log('Found UID higher up in the tree');
                    real_uid = (el as HTMLElement).dataset.uid
                } else {
                    throw "No parent with [data-uid] set"
                }
            }
        }

        if (!real_uid) {
            console.warn(this.outerHTML);
            throw `UID required`
        }

        // console.log(real_uid);
        this.uid = real_uid;
        this.dataset.uid = real_uid;

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

    /** While abstract for this, @emph{must} be overridden for everyone else */
    abstract redraw(data: VEvent): void

}
