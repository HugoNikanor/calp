window.formatters.set('description', (el, d) => {
    if (/<\/?\w+( +\w+(=["']?\w+["']?)?)* *\/?>/.exec(d)) {
        /* Assume that the text is HTML if it contains something which looks
           like an HTML tag */
        let parser = new DOMParser();
        let doc = parser.parseFromString(d, 'text/html');
        el.replaceChildren(doc.body);
    } else {
        /* Otherwise it should be plain(er) text, parse "all" links */
        let rx = /https?:\/\/\S+/g
        let idx = 0;
        let children = []
        for (let match of d.matchAll(rx)) {
            let anch = document.createElement('a')
            anch.href = match[0]
            anch.textContent = match[0]

            children.push(d.substring(idx, match.index))
            children.push(anch)

            idx = match.index + match[0].length
        }
        children.push(d.substring(idx))
        el.replaceChildren(...children);
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
