// https://stackoverflow.com/questions/376373/pretty-printing-xml-with-javascript/


const URI_BASE = `${window.location.protocol}//${window.location.host}`
const CALDAV = "urn:ietf:params:xml:ns:caldav"

let xsltProcessor

(async function () {
  const xslt = parseXML(await fetch('prettify.xsl').then(r => r.text()))
  xsltProcessor = new XSLTProcessor()
  xsltProcessor.importStylesheet(xslt)
})()

let current_document = null

function iterator_to_list(it) {
  let result
  const dest = []
  while ((result = it.iterateNext())) {
    dest.push(result)
  }
  return dest
}

/** Given a string containing XML content, return a document */
function parseXML(s) {
  return new DOMParser().parseFromString(s, 'application/xml')
}

/** Given an XML document, return a string containing a serialized form of that document */
function serializeXML(doc) {
  return new XMLSerializer().serializeToString(doc)
}

/** Rewrite a given XML document to be indented for human consumption.
    Returns a neow XML document.
 */
function prettifyXML(doc) {
  return xsltProcessor.transformToDocument(doc)
}

async function form_handler_common(
  e /* SubmitEvent */,
  response_body_el /* HTMLElement */,
  preparer /* FormData -> RequestInit */
) /* : void */ {
  e.preventDefault()
  const data = new FormData(e.target)

  current_document = null

  {
    const progress = document.createElement('progress')
    response_body_el.replaceChildren(progress)
  }

  const request_init = preparer(data)

  let response
  try {
    response = await fetch(`${URI_BASE}${data.get('href')}`, request_init)
  } catch (e) {
    const pre = document.createElement('pre')
    pre.classList.add('error')
    pre.textContent = `${e}`
    response_body_el.replaceChildren(pre)
    return
  }

  document.getElementById('http-status')
    .textContent = `${response.status} ${response.statusText}`

  document.getElementById('http-headers')
    .replaceChildren(...[...response.headers].flatMap(p => {
      const dl = document.createElement('dl')
      const dd = document.createElement('dd')
      dl.textContent = p[0]
      dd.textContent = p[1]
      return [dl, dd]
    }))

  if (request_init.method === 'HEAD') {
    response_body_el.replaceChildren()
    return
  }

  /* Content type checks all have an open end, to ignore any parameters */
  const content_type = response.headers.get('Content-Type')?.toLowerCase()
  if (content_type?.startsWith('image/')) {
    const img = document.createElement('img')
    img.style['max-width'] = '100%'
    img.src = URL.createObjectURL(await response.blob())
    response_body_el.replaceChildren(img)
  } else if (content_type?.match(/^application\/calendar\+json/)) {
    const pre = document.createElement('pre')
    pre.textContent = format_jcal_object(await response.json()).join('\n')
    response_body_el.replaceChildren(pre)
  } else if (
    content_type?.match(/^application\/(.*)\+json/)
      || content_type?.startsWith('application/json')) {
    const pre = document.createElement('pre')
    pre.textContent = JSON.stringify(await response.json(), null, 2)
    response_body_el.replaceChildren(pre)
  } else if (
    content_type?.match(/^application\/(.*)\+xml/)
      || content_type?.startsWith('application/xml')) {

    const raw_response = await response.text()
    current_document = parseXML(raw_response)

    switch (data.get('xml-output-format')) {
    case 'xsl': {
      /* use XSLT to pretty print document (doesn't work in firefox) */
      const pre = document.createElement('pre')
      pre.textContent = serializeXML(prettifyXML(current_document))
      response_body_el.replaceChildren(pre)
      break
    }
    case 'html': {
      /* Generate an interactive HTML tree of the document */
      response_body_el.replaceChildren(build_tree(current_document.documentElement))
      break
    }
    case 'raw':
    default: {
      const pre = document.createElement('pre')
      pre.textContent = raw_response
      response_body_el.replaceChildren(pre)
      break
    }
    }

  } else if (content_type?.match(/^text\/html/)) {
    const iframe = document.createElement('iframe')
    // TODO a blob uri could also work
    iframe.srcdoc = await response.text()
    iframe.style.width = '100%'
    iframe.style.minHeight = '20em'
    response_body_el.replaceChildren(iframe)
  } else if (content_type?.startsWith('text/')) {
    const pre = document.createElement('pre')
    pre.textContent = await response.text()
    response_body_el.replaceChildren(pre)
  } else {
    const pre = document.createElement('pre')
    // TODO possibly base-64 encode the data, and show that

    const a = document.createElement('a')
    a.setAttribute('target', '_blank')
    const blob = await response.blob()
    a.href = URL.createObjectURL(blob)
    if (content_type) {
      a.textContent = `Show binary data of type ${content_type} (${blob.size} bytes)`
    } else {
      a.textContent = `Show binary data of unknown type (${blob.size} bytes)`
    }

    response_body_el.replaceChildren(a)

  }


}

async function main() {

  if (navigator.userAgent.match(/Gecko\/\d+/)) {
    const div = document.createElement('div')
    div.classList.add('warning')
    div.textContent = `
WARNING: Gecko based browsers (Firefox) doesn't support pretty-printing XML as of 2025-10-30.
Consider using a browser with a different engine. Chromium browsers are known to work. `
    document.getElementById('warnings').appendChild(div)
  }

  {
    /* https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Reference/Roles/tab_role#example */
    const tabsContainer = document.querySelector('.tabs')
    const tabList = tabsContainer.querySelector(':scope > [role="tablist"]')
    const tabs = Array.from(tabList.querySelectorAll(':scope > [role="tab"]'))
    const tabPanelsContainer = tabsContainer.querySelector(':scope > .tab-panels')
    const tabPanels = Array.from(tabPanelsContainer.querySelectorAll(':scope > [role="tabpanel"]'))

    function showTab(targetTab) {
      for (const tab of tabs) {
        if (tab === targetTab) continue
        tab.setAttribute('aria-selected', false)
        tab.tabIndex = -1
      }

      targetTab.setAttribute('aria-selected', true)
      targetTab.tabIndex = 0

      window.location.hash = `tab=${targetTab.id}`

      const targetPanel = document.getElementById(targetTab.getAttribute('aria-controls'))
      for (const panel of tabPanels) {
        if (panel === targetPanel) continue
        panel.hidden = true
      }
      targetPanel.hidden = false

      const form_id = targetPanel.querySelector('form').id
      for (const input of document.querySelectorAll('#form-common [name]')) {
        input.setAttribute('form', form_id)
      }
    }

    tabList.addEventListener('keydown', (e) => {
      const currentTab = e.target
      const currentIndex = tabs.indexOf(currentTab)
      if (currentIndex === -1) return
      let newIndex = 0
      switch (e.key) {
      case 'ArrowRight':
        newIndex = (currentIndex + 1) % tabs.length
        break
      case 'ArrowLeft':
        newIndex = (currentIndex - 1 + tabs.length) % tabs.length
        break
      case 'Home':
        newIndex = 0
        break
      case 'End':
        newINdex = tabs.length - 1
        break
      default:
        return
      }

      e.preventDefault()
      e.stopPropagation()
      tabs[newIndex].focus()
    })

    tabs.forEach((tab) => {
      tab.addEventListener('click', (e) => showTab(e.target))
      tab.addEventListener('keydownn', (e) => {
        if (e.key === 'Enter' || e.key == ' ') {
          e.preventDefault()
          e.stopPropagation()
          showTab(e.target)
        }
      })
    })
  }

  {
    const hash = new URLSearchParams(window.location.hash.substring(1))
    const tab = hash.get('tab')
    if (tab) {
      const tab_el = document.getElementById(tab)
      if (tab_el?.role === 'tab') {
        tab_el.click()
      }
    } else {
      document.getElementById('propfind-tab').click()
    }
  }

  const response_body_el = document.getElementById('response-body')

  document.getElementById('xpath-form').addEventListener('submit', async (e) => {
    e.preventDefault()
    if (! current_document) return

    const data = new FormData(e.target)
    const pre = document.createElement('pre')

    try {
      const iterator = current_document.evaluate(
        data.get('xpath'),
        current_document,
        current_document.documentElement,
        XPathResult.UNORDERED_NODE_ITERATOR_TYPE)

      // TODO this should create an interactive tree, just like the
      // non-filtered response (if that setting is used)
      for (const match of iterator_to_list(iterator)) {
        if (match.nodeType == Node.ELEMENT_NODE) {
          pre.textContent += serializeXML(prettifyXML(match)) + '\n'
        } else {
          pre.textContent += match.nodeValue + '\n'
        }
      }

      response_body_el.replaceChildren(pre)
    } catch (e) {
      pre.classList.add('error')
      pre.textContent = `${e}`
    }

    response_body_el.replaceChildren(pre)
  })

  document.getElementById('propfind-form').addEventListener('submit', (e) => {
    return form_handler_common(e, response_body_el, (data) => ({
      method: 'PROPFIND',
      headers: {
        'Depth': data.get('depth'),
        'Content-Type': 'application/xml',
      },
      // TODO if the request-body field is empty (after trimming whitespace)
      // then no body should be sent, and the content-type header MUST be omitted.
      // This is equivalent to a request body of
      // `<propfind xmlns="DAV:"><allprop/></propfind>`
      body: data.get('request-body'),
    }))
  })

  document.getElementById('get-form').addEventListener('submit', (e) => {
    return form_handler_common(e, response_body_el, (data) => ({
      method: data.get('head') ? 'HEAD' : 'GET',
      headers: {
        'Accept': data.get('accept') || '*/*',
      },
    }))
  })

  document.getElementById('options-form').addEventListener('submit', (e) => {
    return form_handler_common(e, response_body_el, (data) => ({
      method: 'OPTIONS',
    }))
  })

  document.getElementById('report-form').addEventListener('submit', (e) => {
    return form_handler_common(e, response_body_el, (data) => ({
      method: 'REPORT',
      headers: {
        'Depth': data.get('depth'),
        'Content-Type': 'application/xml',
      },
      body: data.get('request-body'),
    }))
  })

  /* All anon-implemented forms. Remove items from this array as they are added above */
  for (const tab of ['proppatch', 'put', 'delete', 'mkcol', 'copy', 'mkcalendar', 'lock', 'unlock']) {
    document.getElementById(`${tab}-form`).addEventListener('submit', (e) => {
      e.preventDefault()
    })
  }
}

window.addEventListener('load', () => main())


function build_tree(el) {

  /*
    TODO custom right click actions:
    - collapse all children
    - expand all children
    */

  switch (el.nodeType) {
  case XMLDocument.TEXT_NODE: {
    const pre = document.createElement('pre')
    pre.textContent = el.textContent
    return pre
  }
  case XMLDocument.ELEMENT_NODE: {
    if (el.attributes.length === 0 && (el.childNodes.length === 0 || (el.childNodes.length === 1 && el.childNodes[0].nodeType === XMLDocument.TEXT_NODE && el.childNodes[0].textContent.length <= 40))) {
      const desc = document.createElement('div')
      // 1em copied from style.css (TODO unifi these)
      // 1.5ch is the width of the <details /> arrow (in firefox)
      desc.style.paddingLeft = 'calc(1em + 1.5ch)';
      const summ = document.createElement('summary')
      const content = document.createElement('pre')
      summ.style.display = 'inline-block'
      content.style.display = 'inline-block'
      summ.textContent = `<${el.tagName}`
      if (el.childNodes.length === 1) {
        content.textContent = el.childNodes[0].textContent
      }
      desc.replaceChildren(summ, content)
      return desc
    } else {
      const desc = document.createElement('details')
      desc.setAttribute('open', 'open')
      const summ = document.createElement('summary')
      /*
        TODO if the tag has
        - no attributes,
        - only text content,
        - no newlines in the text, and
        - sufficiently short text
        then don't create a details tag, but insert the items inline (on the same line)
        This would save quite a bit of horizontal space.

        TODO similarly, tags with no attributes AND no content shouldn't be exandable at all
      */
      summ.textContent = `<${el.tagName}`
      const dl = document.createElement('dl')
      dl.replaceChildren(...[...el.attributes].flatMap(attribute => {
        const dt = document.createElement('dt')
        const dd = document.createElement('dd')
        dt.textContent = attribute.name
        dd.textContent = attribute.value
        return [dt, dd]
      }))

      if (el.namespaceURI === CALDAV && el.localName === 'calendar-data'
          && el.attributes["content-type"]
          && el.childNodes.length === 1 && el.childNodes[0].nodeType === XMLDocument.TEXT_NODE)
      {
        const text = el.childNodes[0].textContent
        switch (el.attributes["content-type"].textContent) {
        case 'application/calendar+xml': {
          const nested_document = parseXML(text)
          const inner_desc = document.createElement('details')
          const inner_summ = document.createElement('summary')
          inner_summ.classList.add('sub-document-header')
          inner_summ.textContent = '#application/calendar+xml'
          inner_desc.replaceChildren(inner_summ, build_tree(nested_document.documentElement))
          desc.replaceChildren(summ, dl, inner_desc)
          break
        }

        case 'application/calendar+json': {
          const pre = document.createElement('pre')
          pre.textContent = format_jcal_object(JSON.parse(text)).join('\n')
          desc.replaceChildren(summ, dl, pre)
          break
        }

        default: {
          desc.replaceChildren(summ, dl, ...[...el.childNodes].map(build_tree))
          break
        }
        }
      } else {
        desc.replaceChildren(summ, dl, ...[...el.childNodes].map(build_tree))
      }
      return desc
    }
  }
  default:
    return el
  }
}


const indent = (n) => (s) => s.padStart(s.length + n)

function format_jcal_object(o) /* : [string] */{

  /* TODO commas */
  const children = o[2].flatMap(c => format_jcal_object(c).map(indent(3)))
  const child_element = children.length === 0 ? [' []]'] : [' [', ...children, ' ]]']

  return [
    `["${o[0]}",`,
    ` [`,
    /* TODO commas */
    ...o[1].flatMap(p => format_jcal_property(p).map(indent(3))),
    ` ],`,
    ...child_element,
  ]
}


function format_jcal_property(p) /* : [string] */ {
  // TODO parameters
  // TODO newline for value, if it's really long
  return [`["${p[0]}", {}, "${p[2]}", ${JSON.stringify(p[3])}]`]
}
