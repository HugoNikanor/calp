export { TabElement }

/* <tab-element /> */
class TabElement extends HTMLElement {
    constructor() {
        super();
    }

    connectedCallback() {
        let template
            = (document.getElementById('tab-template') as HTMLTemplateElement)
                .content
        // const shadowRoot = this.attachShadow({ mode: 'open' })
        //     .appendChild(template.cloneNode(true));

        let content = Array.from(this.children, (e) => e.cloneNode(true))

        this.replaceChildren(template.cloneNode(true));

        let label = this.querySelector('label')
        if (!label) throw "Invalid tab"

        label.setAttribute('title', this.getAttribute('label-title') || '')
        label.innerText = this.getAttribute('label') || 'T'

        this.querySelector('slot[name="content"]')!.replaceWith(...content);
    }
}
