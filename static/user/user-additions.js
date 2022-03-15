window.formatters.set('description', (el, d) => {
    if (/<br\/?>/.exec(d)) {
        /* Assume that the text is HTML iff in contains a <br/> tag */
        let parser = new DOMParser();
        let doc = parser.parseFromString(d, 'text/html');
        el.replaceChildren(doc.body);
    } else {
        /* Otherwise it should be plain(er) text, parse "all" links */
        el.innerHTML = d.replaceAll(/https?:\/\/\S+/g, '<a href="$&">$&</a>');
    }
})
