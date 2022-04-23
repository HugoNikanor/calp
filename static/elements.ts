import { ComponentDescription } from './components/vevent-description'
import { ComponentEdit } from './components/vevent-edit'
import { VEventDL } from './components/vevent-dl'
import { ComponentBlock } from './components/vevent-block'
import { DateTimeInput } from './components/date-time-input'
import { PopupElement } from './components/popup-element'
import { InputList } from './components/input-list'
import { EditRRule } from './components/edit-rrule'
import { TabGroupElement } from './components/tab-group-element'
import { VEventChangelog } from './components/changelog'
import { SliderInput } from './components/slider'
import { DateJump } from './components/date-jump'

export { initialize_components }

function initialize_components() {


    /* These MUST be created AFTER vcal_objcets and event_calendar_mapping are
    inistialized, since their constructors assume that that piece of global
    state is available */
    customElements.define('vevent-description', ComponentDescription);
    customElements.define('vevent-edit', ComponentEdit);
    customElements.define('vevent-dl', VEventDL);
    customElements.define('vevent-block', ComponentBlock);
    customElements.define('vevent-edit-rrule', EditRRule);

    /* date-time-input should be instansiatable any time, but we do it here
    becouse why not */

    customElements.define('date-time-input', DateTimeInput /*, { extends: 'input' } */)
    customElements.define('input-list', InputList);
    customElements.define('slider-input', SliderInput);
    customElements.define('date-jump', DateJump);

    /* These maybe also require that the global maps are initialized */
    customElements.define('popup-element', PopupElement)
    customElements.define('tab-group', TabGroupElement)
    customElements.define('vevent-changelog', VEventChangelog);
}
