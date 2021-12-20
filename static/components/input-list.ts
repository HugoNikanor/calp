export { InputList }

/* This file replaces input_list.js */

/*
  TODO allow each item to be a larger unit, possibly containing multiple input
  fields.
*/
class InputList extends HTMLElement {

    el: HTMLInputElement;

    private _listeners: [string, (e: Event) => void][] = [];

    constructor() {
        super();
        this.el = this.children[0].cloneNode(true) as HTMLInputElement;
    }

    connectedCallback() {
        for (let child of this.children) {
            child.remove();
        }
        this.addInstance();
    }

    createInstance(): HTMLInputElement {
        let new_el = this.el.cloneNode(true) as HTMLInputElement
        let that = this;
        new_el.addEventListener('input', function() {
            /* TODO .value is empty both if it's actually empty, but also
               for invalid input. Check new_el.validity, and new_el.validationMessage
            */
            if (new_el.value === '') {
                let sibling = (this.previousElementSibling || this.nextElementSibling)
                /* Only remove ourselves if we have siblings
                   Otherwise we just linger */
                if (sibling) {
                    this.remove();
                    (sibling as HTMLInputElement).focus();
                }
            } else {
                if (!this.nextElementSibling) {
                    that.addInstance();
                    // window.setTimeout(() => this.focus())
                    this.focus();
                }
            }
        });

        for (let [type, proc] of this._listeners) {
            new_el.addEventListener(type, proc);
        }

        return new_el;
    }

    addInstance() {
        let new_el = this.createInstance();
        this.appendChild(new_el);
    }

    get value(): any[] {
        let value_list = []
        for (let child of this.children) {
            value_list.push((child as any).value);
        }
        if (value_list[value_list.length - 1] === '') {
            value_list.pop();
        }
        return value_list
    }

    set value(new_value: any[]) {

        let all_equal = true;
        for (let i = 0; i < this.children.length; i++) {
            let sv = (this.children[i] as any).value
            all_equal
                &&= (sv == new_value[i])
                || (sv === '' && new_value[i] == undefined)
        }
        if (all_equal) return;

        /* Copy our current input elements into a dictionary.
           This allows us to only create new elements where needed
        */
        let values = new Map;
        for (let child of this.children) {
            values.set((child as HTMLInputElement).value, child);
        }

        let output_list: HTMLInputElement[] = []
        for (let value of new_value) {
            let element;
            /* Only create element if needed */
            if ((element = values.get(value))) {
                output_list.push(element)
                /* clear dictionary */
                values.set(value, false);
            } else {
                let new_el = this.createInstance();
                new_el.value = value;
                output_list.push(new_el);
            }
        }
        /* final, trailing, element */
        output_list.push(this.createInstance());

        this.replaceChildren(...output_list);
    }

    addEventListener(type: string, proc: ((e: Event) => void)) {
        // if (type != 'input') throw "Only input supported";

        this._listeners.push([type, proc])

        for (let child of this.children) {
            child.addEventListener(type, proc);
        }
    }
}
