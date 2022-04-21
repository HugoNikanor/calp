export { SliderInput }

import { makeElement } from '../lib'

const dflt = {
    min: 0,
    max: 100,
    step: 1,
}

type Attribute = 'min' | 'max' | 'step'

class SliderInput extends HTMLElement {

    /* value a string since javascript kind of expects that */
    #value = "0";
    min = 0;
    max = 100;
    step = 1;

    readonly slider: HTMLInputElement;
    readonly textIn: HTMLInputElement;

    constructor(min?: number, max?: number, step?: number, value?: number) {
        super();

        this.min = min || parseFloat(this.getAttribute('min') || ""+dflt['min']);
        this.max = max || parseFloat(this.getAttribute('max') || ""+dflt['max']);
        this.step = step || parseFloat(this.getAttribute('step') || ""+dflt['step']);
        // https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input/range#value
        const defaultValue
            = (this.max < this.min)
            ? this.min
            : this.min + (this.max - this.min)/2;

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

        this.slider.addEventListener('input', (e) => this.propagate(e));
        this.textIn.addEventListener('input', (e) => this.propagate(e));

        /* MUST be after sub components are bound */
        this.value = "" + (value || this.getAttribute('value') || defaultValue);
    }

    connectedCallback() {
        this.replaceChildren(this.slider, this.textIn);
    }


    static get observedAttributes(): Attribute[] {
        return ['min', 'max', 'step']
    }

    attributeChangedCallback(name: Attribute, _: string|null, to: string|null): void {
        if (to) {
            this.slider.setAttribute(name, to);
            this.textIn.setAttribute(name, to);
        } else {
            this.slider.removeAttribute(name);
            this.textIn.removeAttribute(name);
        }
        this[name] = parseFloat(to || ""+dflt[name])
    }

    propagate(e: Event) {
        this.value = (e.target as HTMLInputElement).value;
        if (e instanceof InputEvent && this.oninput) {
            this.oninput(e);
        }
    }

    set value(value: string) {
        this.slider.value = value;
        this.textIn.value = value;
        this.#value = value;
    }

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
