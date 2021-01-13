/*
  ∀ children('.input-list') => 'unit' ∈ classList(child)

  <div class="input-list">
    <div class="unit"><input/></div>
    <div class="unit final"><input/></div>
  </div>

*/


/* private */
function transferListeners(old_unit, new_unit) {
    for (let [o, n] of zip([old_unit, ...old_unit.querySelectorAll("*")],
                           [new_unit, ...new_unit.querySelectorAll("*")])) {
        for (const key in o.listeners) {
            if (! o.listeners.hasOwnProperty(key)) continue;
            for (let proc of o.listeners[key]) {
                n.addEventListener(key, proc);
            }
        }
    }
}


/* private */
function advance_final(input_list) {
    let old_unit = input_list.unit;
    let new_unit = old_unit.cloneNode(true);
    new_unit.classList.add('final');
    transferListeners(old_unit, new_unit);
    input_list.appendChild(new_unit);
}


/* private */
function update_inline_list () {

    /* can target self */
    let unit = this.closest('.unit');

    let lst = this.closest('.input-list');

    if (unit.classList.contains("final")) {
        if (this.value !== '') {
            unit.classList.remove('final');
            advance_final(lst);
        }
    } else {
        /* TODO all significant fields empty, instead of just current */
        if (this.value === '') {
            let sibling = unit.previousElementSibling || unit.nextElementSibling;
            unit.remove();
            if (sibling.tagName !== 'input')
                sibling = sibling.querySelector('input');
            sibling.focus();
        }
    }
}

/* run this from window.onload (or similar) */
function init_input_list() {

    for (let lst of document.getElementsByClassName('input-list')) {

        for (let el of lst.getElementsByTagName('input')) {
            el.addEventListener('input', update_inline_list);
        }

        let oldUnit = lst.querySelector('.final.unit')
        let unit = oldUnit.cloneNode(true);

        transferListeners(oldUnit, unit);

        lst.unit = unit;

        if (lst.dataset.bindby) {
            lst.get_value = lst.dataset.bindby;
        } else if (lst.dataset.joinby) {
            lst.get_value = get_get_value(lst.dataset.joinby);
        } else {
            lst.get_value = get_get_value();
        }

        /* Propagate add event listener downwards */
        lst._addEventListener = lst.addEventListener;
        lst.addEventListener = function(type, proc) {
            switch (type) {
            case 'input':
                for (let el of lst.getElementsByTagName('input')) {
                    el.addEventListener('input', proc);
                }
            default:
                lst._addEventListener(type, proc);
            }
        };
    }
}

/* -------------------------------------------------- */

/* different function forms since we want to capture one self */
const get_get_value = (join=',') => function () {
    return [...this.querySelectorAll('input')]
        .map(x => x.value)
        .filter(x => x != '');
        // .join(join);
}

/* -------------------------------------------------- */
