/**
   <slider-input />

   A Web Component implementing a slider with a corresponding number input.

   TODO rename this file

   ### Parameters

   All of these are optional, see {@linkcode dflt} for defaults.

   #### min
   Minimum allowed value.

   #### max
   Maximum allowed value.

   #### step
   How large each step of the slider/number box should be.

   @module
*/

export { SliderInput, Attribute, dflt }

import { makeElement } from '../lib'

/** Defalut values for all attributes, if not given */
const dflt = {
    min: 0,
    max: 100,
    step: 1,
}

/** Valid attributes for SliderInput */
type Attribute = 'min' | 'max' | 'step'

/**
   Component displaying an input slider, together with a corresponding numerical
   input
*/
class SliderInput extends HTMLElement {

    /* value a string since javascript kind of expects that */
    #value = "" + dflt.min
    /** Minimum allowed value */
    min = dflt.min
    /** Maximum allowed value */
    max = dflt.max
    /** How large each step should be */
    step = dflt.step

    /** The HTML slider component */
    readonly slider: HTMLInputElement;
    /** The HTML number input component */
    readonly textIn: HTMLInputElement;

    constructor(min?: number, max?: number, step?: number, value?: number) {
        super();

        this.min = min || parseFloat(this.getAttribute('min') || "" + dflt['min']);
        this.max = max || parseFloat(this.getAttribute('max') || "" + dflt['max']);
        this.step = step || parseFloat(this.getAttribute('step') || "" + dflt['step']);
        // https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/range#value
        const defaultValue
            = (this.max < this.min)
                ? this.min
                : this.min + (this.max - this.min) / 2;

        this.slider = makeElement('input', {
            type: 'range',
            min: this.min,
            max: this.max,
            step: this.step,
            value: this.value,
        }) as HTMLInputElement
        this.textIn = makeElement('input', {
            type: 'number',
            min: this.min,
            max: this.max,
            step: this.step,
            value: this.value,
        }) as HTMLInputElement

        this.slider.addEventListener('input', e => this.#propagate(e));
        this.textIn.addEventListener('input', e => this.#propagate(e));

        /* MUST be after sub components are bound */
        this.value = "" + (value || this.getAttribute('value') || defaultValue);
    }

    connectedCallback() {
        this.replaceChildren(this.slider, this.textIn);
    }

    /** ['min', 'max', 'step'] */
    static get observedAttributes(): Attribute[] {
        return ['min', 'max', 'step']
    }

    attributeChangedCallback(name: Attribute, _?: string, to?: string): void {
        if (to) {
            this.slider.setAttribute(name, to);
            this.textIn.setAttribute(name, to);
        } else {
            this.slider.removeAttribute(name);
            this.textIn.removeAttribute(name);
        }
        this[name] = parseFloat(to || "" + dflt[name])
    }

    /**
       Helper for updating the value attribute

       Event listeners are bound on both the input elements, which both simply
       call this. This procedure then updates the classes value field.

       TODO `oninput`?
    */
    #propagate(e: Event) {
        this.value = (e.target as HTMLInputElement).value;
        if (e instanceof InputEvent && this.oninput) {
            this.oninput(e);
        }
    }

    /**
       Set a new numerical value.

       A number not possible due to the current `min`, `max`, and `step`
       properties can be set and will work, the slider will however not
       properly show it, but rather the closest value it can display.
     */
    set value(value: string) {
        this.slider.value = value;
        this.textIn.value = value;
        this.#value = value;
    }

    /** Get the current numerical value */
    get value(): string {
        return this.#value;
    }

    /* TODO do we want to implement this?
     * oninput directly on the component already works
     * /
    addEventListener(type: string, proc: ((e: Event) => void)) {
    }
    */
}
