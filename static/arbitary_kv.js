/*
  The freeform key-value fields at the bottom of any popup.
 */

function init_arbitary_kv() {
    for (let el of document.getElementsByClassName("newfield")) {
        let [name, type_selector, value_field] = el.children;

        /* TODO list fields */
        /* TODO add and remove fields. See update_inline_list */

        function update_value_field (el) {
            let [name_field, type_selector, value_field] = el.children;


            let value = makeElement('input');
            let values = [value];


            switch (name_field.value.toUpperCase()) {
            case 'GEO':
                value.type = 'number';
                values.push(makeElement('input', {
                    type: 'number',
                }));
                break;

            case 'CLASS':
                // Add auto completion
                break;

            case 'ACTION':
                // Add auto completion
                break;

            case 'TRANSP':
                // Replace with toggle betwen OPAQUE and TRANSPARENT
                break;

            case 'PERCENT-COMPLETE':
                value.min = 0;
                value.max = 100;
                break;

            case 'PRIORITY':
                value.min = 0;
                value.max = 9;
                break;

            default:


                switch (type_selector.options[type_selector.selectedIndex].value) {
                case 'integer':
                case 'float':
                    value.type = 'number';
                    break;

                case 'uri':
                    value.type = 'url';
                    break;

                case 'binary':
                    value.type = 'file';
                    break;

                case 'date-time':
                    values.push(makeElement('input', {
                        type: 'time',
                    }));
                    /* fallthrough */
                case 'date':
                    value.type = 'date';
                    break;

                case 'cal-address':
                    value.type = 'email';
                    break;

                case 'utc-offset':
                    value.type = 'time';
                    let lbl = makeElement('label');
                    let id = gensym();

                    lbl.setAttribute('for', id);

                    /* TODO make these labels stand out more */
                    lbl.appendChild(makeElement('span', {
                        className: 'plus',
                        innerText: '+',
                    }));
                    lbl.appendChild(makeElement('span', {
                        className: 'minus',
                        innerText: '-',
                    }));
                    values.splice(0,0,lbl);
                    values.splice(0,0, makeElement('input', {
                        type: 'checkbox',
                        style: 'display:none',
                        className: 'plusminuscheck',
                        id: id,
                    }));
                    break;

                case 'boolean':
                    value.type = 'checkbox';
                    break;

                case 'period':
                    value.type = 'text';
                    // TODO validate /P\d*H/ typ
                    break;

                case 'recur':
                    // TODO
                default:
                    value.type = 'text';
                }
            }


            value_field.innerHTML = '';
            for (let v of values) {
                console.log(v);
                value_field.appendChild(v);
            }
        }

        name.addEventListener('input', function setOptionDropdown () {
            let types = valid_input_types[this.value.toUpperCase()];
            let el = this.parentElement;
            let [_, type_selector, value_field] = el.children;

            type_selector.disabled = false;
            if (types) {
                type_selector.innerHTML = '';
                for (let type of types) {
                    type_selector.appendChild(
                        makeElement('option', { value: type, innerText: type }))
                }
                if (types.length == 1) {
                    type_selector.disabled = true;
                }
            } else {
                type_selector.innerHTML = '';
                for (let type of all_types) {
                    type_selector.appendChild(
                        makeElement('option', { value: type, innerText: type }))
                }
            }

            update_value_field(el);
        });

        type_selector.addEventListener('change', function () {
            update_value_field(this.parentElement);
        });
    }

}
