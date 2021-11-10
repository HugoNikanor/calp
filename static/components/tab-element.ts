export { TabElement }

/* <tab-element /> */
class TabElement extends HTMLElement {
    constructor() {
        super();
    }

    connectedCallback() {
        // this.replaceChildren(template.cloneNode(true));
        let template
            = (document.getElementById('tab-template') as HTMLTemplateElement)
                .content
        // const shadowRoot = this.attachShadow({ mode: 'open' })
        //     .appendChild(template.cloneNode(true));
        // console.log(this);
        let label = this.querySelector('[slot="label"]')
        let content = this.querySelector('[slot="content"]')
        if (!verifySlot(label)) throw "Bad label";
        if (!verifySlot(content)) throw "Bad content";

        /* TODO set label hover title somewhere around here */

        this.replaceChildren(template.cloneNode(true));
        this.querySelector('slot[name="label"]')!.replaceWith(label);
        this.querySelector('slot[name="content"]')!.replaceWith(content);
    }
}

function verifySlot(el: Node | null): el is HTMLElement {
    if (el === null) {
        console.error("Element is null");
        return false;
    }
    if (!(el instanceof HTMLElement)) {
        console.error("Node is not an HTMLElement", el);
        return false;
    }
    return true
}
