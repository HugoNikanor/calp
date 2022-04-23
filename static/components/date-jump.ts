export { DateJump }

/* Replace backend-driven [today] link with frontend, with one that
    gets correctly set in the frontend. Similarly, update the go to
    specific date button into a link which updates wheneven the date
    form updates.
*/
class DateJump extends HTMLElement {

    readonly golink: HTMLAnchorElement;
    readonly input: HTMLInputElement;

    constructor() {
        super();

        this.golink = document.createElement('a')
        this.golink.classList.add('btn');
        this.golink.textContent = "➔"
        this.input = document.createElement('input')
        this.input.type = 'date';
    }

    connectedCallback() {

        /* Form is just here so the css works out */
        let form = document.createElement('form');
        form.replaceChildren(this.input, this.golink);
        this.replaceChildren(form);

        this.input.onchange = () => {
            let date = this.input.valueAsDate!.format('~Y-~m-~d');
            this.golink.href = `${date}.html`
        }

        let now = (new Date).format("~Y-~m-~d")
        this.input.value = now;
        /* onchange isn't triggered by manually setting the value */
        this.golink.href = `${now}.html`
    }
}
