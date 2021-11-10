import { ComponentDescription } from './components/vevent-description'
import { ComponentEdit } from './components/vevent-edit'
import { VEventDL } from './components/vevent-dl'
import { ComponentBlock } from './components/vevent-block'
import { DateTimeInput } from './components/date-time-input'
import { PopupElement } from './components/popup-element'
import { TabElement } from './components/tab-element'

export { initialize_components }

function initialize_components() {


    /* These MUST be created AFTER vcal_objcets and event_calendar_mapping are
    inistialized, since their constructors assume that that piece of global
    state is available */
    customElements.define('vevent-description', ComponentDescription);
    customElements.define('vevent-edit', ComponentEdit);
    customElements.define('vevent-dl', VEventDL);
    customElements.define('vevent-block', ComponentBlock);

    /* date-time-input should be instansiatable any time, but we do it here
    becouse why not */

    customElements.define('date-time-input', DateTimeInput /*, { extends: 'input' } */)

    /* These maybe also require that the global maps are initialized */
    customElements.define('popup-element', PopupElement)
    customElements.define('tab-element', TabElement)
}
