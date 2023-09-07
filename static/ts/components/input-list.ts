/**
 * `<input-list />`
 *
 * A list of identical input fields, which forms a group. For example
 * useful to handle keywords.
 *
 * @category Web Components
 * @mergeTarget components
 * @module
 */
export { InputList }

/*
  TODO allow each item to be a larger unit, possibly containing multiple input
  fields.
*/
/**
   A multi-valued input, done by creating extra input fields as needed.

   The first element of body MUST be an input element, which will be used as the
   template for each instance. A tag input could for example look like

   @example
   ```html
   <input-list name="tags">
    <input type="text" placeholder="tag ..." />
   </input-list>
   ```

   Whenever one of the input elements `value` becomes the empty string, that tag
   is removed, and whenever there is no element with the empty string as a
   `value`, a new input element will be added onto the end.
 */
class InputList extends HTMLElement {

    /** The element used as our template. Will be sourced from the initial HTML code. */
    #el: HTMLInputElement;

    /**
       Registered listeners, which will be added onto each created entry

       Keys are event names ('input', 'change', ...) and values event handlers.

       This is a list of tuples rather than a dictionary, since multiple
       listeners of the same type can be registered.
    */
    #listeners: [string, (e: Event) => void][] = [];

    constructor() {
        super();
        this.#el = this.children[0].cloneNode(true) as HTMLInputElement;
    }

    /** Clears all existing children upon mount */
    connectedCallback() {
        for (let child of this.children) {
            child.remove();
        }
        this.#addInstance();
    }

    /**
       Instanciates a new instance of the input element.

       An event listener for 'input' will be added, which will handle the
       addition and removing of other elements.

       All event listeners attachet on the input-list component will also be
       added.
    */
    #createInstance(): HTMLInputElement {
        let new_el = this.#el.cloneNode(true) as HTMLInputElement
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
                    that.#addInstance();
                    // window.setTimeout(() => this.focus())
                    this.focus();
                }
            }
        });

        for (let [type, proc] of this.#listeners) {
            new_el.addEventListener(type, proc);
        }

        return new_el;
    }

    /** Add a new instance of the input element to the container */
    #addInstance() {
        let new_el = this.#createInstance();
        this.appendChild(new_el);
    }

    /**
     * The value from each element, except the last which should always be empty.
     * Has an unspecified type, since children:s value field might give non-strings.
     */
    get value(): unknown[] {
        let value_list = []
        for (let child of this.children) {
            value_list.push((child as any).value);
        }
        if (value_list[value_list.length - 1] === '') {
            value_list.pop();
        }
        return value_list
    }

    /**
       Overwrite the current value with a new one.

       Each entry in the array will be mapped unto one instance of the template
       input element. A final empty element will also be added.
     */
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
                let new_el = this.#createInstance();
                new_el.value = value;
                output_list.push(new_el);
            }
        }
        /* final, trailing, element */
        output_list.push(this.#createInstance());

        this.replaceChildren(...output_list);
    }

    /**
       Add an event listener to each of the inputs.

       This basically works as the "regular" version.
     */
    addEventListener(type: string, proc: ((e: Event) => void)) {
        // if (type != 'input') throw "Only input supported";

        this.#listeners.push([type, proc])

        for (let child of this.children) {
            child.addEventListener(type, proc);
        }
    }
}
