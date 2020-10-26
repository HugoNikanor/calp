/*
  TODO document 'input-list'.

  ∀ children('.input-list') => 'unit' ∈ classList(child)

  <div class="input-list">
    <div class="unit"><input/></div>
    <div class="unit final"><input/></div>
  </div>

*/


function transferListeners(old_unit, new_unit) {
    for (let [o, n] of zip(old_unit.querySelectorAll("*"),
                           new_unit.querySelectorAll("*"))) {
        for (const key in o.listeners) {
            if (! o.listeners.hasOwnProperty(key)) continue;
            for (let proc of o.listeners[key]) {
                n.addEventListener(key, proc);
            }
        }
    }
}


function advance_final(input_list) {
    let old_unit = input_list.unit;
    let new_unit = old_unit.cloneNode(true);
    new_unit.classList.add('final');
    transferListeners(old_unit, new_unit);
    input_list.appendChild(new_unit);
}



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
        let oldUnit = lst.querySelector('.final.unit')

        for (let el of lst.getElementsByTagName('input')) {
            el.addEventListener('input', update_inline_list);
        }

        let unit = oldUnit.cloneNode(true);

        transferListeners(oldUnit, unit);

        lst.unit = unit;
    }
}
