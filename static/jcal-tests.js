/* "Test cases" for jcal.js.
   ideally we would actually have runnable tests, but
   `document' is only available in the browser.
*/

let doc = document.implementation.createDocument(xcal, 'icalendar');

jcal = ['key', {}, 'text', 'value'];

jcal_property_to_xcal_property(doc, jcal);



jcal_to_xcal(['vcalendar', [], [['vevent', [['key', {}, 'text', 'value']], []]]]).childNodes[0].outerHTML

/* returns (except not pretty printee)
<icalendar xmlns="urn:ietf:params:xml:ns:icalendar-2.0">
  <vcalendar>
    <properties/>
    <components>
      <vevent>
        <properties>
          <key>
            <text>value</text>
          </key>
        </properties>
        <components/>
      </vevent>
    </components>
  </vcalendar>
</icalendar>
*/
