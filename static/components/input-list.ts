export { InputList }

/* This file replaces input_list.js */

class InputList extends HTMLElement {

    el: HTMLInputElement;

    values: [HTMLInputElement, any][] = [];

    constructor() {
        super();
        this.el = this.children[0].cloneNode(true) as HTMLInputElement;
    }

    connectedCallback() {
        this.addInstance();
    }

    addInstance() {
        let new_el = this.el.cloneNode(true) as HTMLInputElement
        let that = this;
        new_el.addEventListener('input', function() {
            if (new_el.value === '') {
                let sibling = this.previousElementSibling || this.nextElementSibling;
                // this.remove();
                // that.values = that.values.filter((p) => p[0] == this)
                that.values = that.values.filter((p) => p[0] != this);
                this.remove();
                (sibling as HTMLInputElement).focus();
            } else {
                if (!this.nextElementSibling) {
                    that.addInstance();
                    // window.setTimeout(() => this.focus())
                    this.focus();
                }
            }
        });
        this.values.push([new_el, ''])
        // this.appendChild(new_el);
        this.replaceChildren(... this.values.map((p) => p[0]))
    }

    get value(): any[] {
        return []
    }

    set value(new_value: any[]) {
        let els = [];
        for (let value of new_value) {
            let new_el = this.el.cloneNode() as HTMLInputElement;
            new_el.value = value;
            els.push(new_el);
        }
        /* Final element (empty) */
        els.push(this.el.cloneNode() as HTMLInputElement);
        this.replaceChildren(...els);
    }
}
