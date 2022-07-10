window.formatters.set('description', async (el, ev, d) => {
    if (ev.getProperty('X-MICROSOFT-SKYPETEAMSMEETINGURL')) {
        /* parse Microsoft Teams meeting entries */
        /* Replace lines with propper <hr> tags */
        let rx1 = /^_+$/gm
        /* Merge URL:s into a tags */
        let rx2 = /(?<pipe>^|[|])\s*(?<name>[^|<\n]*)<(?<url>[^>]*)>/gm

        let rxs = [`(?<line>${rx1.source})`,
                   `(?<link>${rx2.source})`,
                  ].join('|')
        let rx = new RegExp(`(${rxs})`, 'gm')

        let children = []
        let idx = 0
        for (let match of d.matchAll(rx)) {
            children.push(d.substring(idx, match.index))

            if (match.groups.line) {
                children.push(document.createElement('hr'))
                children.push(document.createElement('br'))
            } else if (match.groups.link) {
                if (match.groups.pipe === '|') {
                    children.push('| ');
                }
                let a = document.createElement('a')
                a.textContent = match.groups.name || match.groups.url
                a.href = match.groups.url
                children.push(a)
            }

            idx = match.index + match[0].length
        }
        children.push(d.substring(idx));
	el.replaceChildren(...children);
    } else if (/<\/?\w+( +\w+(=["']?\w+["']?)?)* *\/?>/.exec(d)) {
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
    .then(resp => { if (! resp.ok) reject("404"); else resp.json() })
    .then(d => resolve(d))
    .catch(err => reject(err))
)


window.formatters.set('location', async function(el, _, d) {
    let rx = /Lokal: (.*)/
    let m = rx.exec(d)
    if (! m) {
        el.textContent = d;
        return;
    }

    try {
        let salar = await window.salar;
    } catch (e) {
        console.warn("Location formatter failed", e);
        return;
    }

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
