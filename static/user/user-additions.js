window.formatters.set('description', (el, d) => {
    if (/<br\/?>/.exec(d)) {
        /* Assume that the text is HTML iff it contains a <br/> tag */
        let parser = new DOMParser();
        let doc = parser.parseFromString(d, 'text/html');
        el.replaceChildren(doc.body);
    } else {
        /* Otherwise it should be plain(er) text, parse "all" links
           (and reserved XML characters)
        */
        // TODO replace with something that doesn't use innerHTML */
        el.innerHTML = d
            .replaceAll(/</g, '&lt;')
            .replaceAll(/>/g, '&gt;')
            .replaceAll(/&/g, '&amp;')
            .replaceAll(/'/g, '&apos;')
            .replaceAll(/"/g, '&quot;')
            .replaceAll(/https?:\/\/\S+/g, '<a href="$&">$&</a>')
    }
})

/* This location formatter is generally not for general use.
   It holds a small lookup table of "all" locations at Linköping University,
   and makes location names from their calendar system clickable.

   To obtain salar.json, run scripts/fetch-liu-map-index.scm from calps source tree.
*/

window.salar = new Promise((resolve, reject) =>
    fetch('/static/user/salar.json')
    .then(d => d.json())
    .then(d => resolve(d)))


window.formatters.set('location', async function(el, d) {
    let rx = /Lokal: (.*)/
    let m = rx.exec(d)
    if (! m) {
        el.textContent = d;
        return;
    }

    let salar = await window.salar;

    let name = m[1]
    let frag = salar[name];
    if (frag) {
        let anch = document.createElement('a');
        anch.href = `https://old.liu.se/karta/${frag}`
        anch.target = '_blank'
        anch.textContent = name;
        el.append('Lokal: ');
        el.append(anch);
    } else {
        el.textContent = `Lokal: ${name}`
    }
})
