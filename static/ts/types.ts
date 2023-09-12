/**
 * Collection of type information for calendar data.
 * @module
 */

export {
    ical_type,
    valid_input_types,
    JCalProperty, JCal,
    xcal, uid,
    ChangeLogEntry
}


/** Name of all valid icalendar types. */
let all_types = [
    'text',
    'uri',
    'binary',
    'float',        /* Number.type = 'float' */
    'integer',      /* Number.type = 'integer' */
    'date-time',    /* Date */
    'date',         /* Date.dateonly = true */
    'duration',     /* TODO */
    'period',       /* TODO */
    'utc-offset',   /* TODO */
    'cal-address',
    'recur',        /* RRule */
    'boolean',      /* boolean */
]


/** The union of all elements in `all_types`. */
type ical_type
    = 'text'
    | 'uri'
    | 'binary'
    | 'float'
    | 'integer'
    | 'date-time'
    | 'date'
    | 'duration'
    | 'period'
    | 'utc-offset'
    | 'cal-address'
    | 'recur'
    | 'boolean'
    | 'unknown'

/**
 * All known names properties (top level keys) can have.
 * Such as "calscale", "dtstart", ...
 */
let property_names = [
    'calscale', 'method', 'prodid', 'version', 'attach', 'categories',
    'class', 'comment', 'description', 'geo', 'location', 'percent-complete',
    'priority', 'resources', 'status', 'summary', 'completed', 'dtend', 'due',
    'dtstart', 'duration', 'freebusy', 'transp', 'tzid', 'tzname', 'tzoffsetfrom',
    'tzoffsetto', 'tzurl', 'attendee', 'contact', 'organizer', 'recurrence-id',
    'related-to', 'url', 'uid', 'exdate', 'exrule', 'rdate', 'rrule', 'action',
    'repeat', 'trigger', 'created', 'dtstamp', 'last-modified', 'sequence', 'request-status'
];


/**
 * Which property fields each component can hold.
 *
 * ```json
 * { 'VCALENDAR': ['PRODID', 'VERSION', 'CALSCALE', 'METHOD'],
 *     ...
 * }
 * ```
 */
let valid_fields: Map<string, string[]> = new Map([
    ['VCALENDAR', ['PRODID', 'VERSION', 'CALSCALE', 'METHOD']],
    ['VEVENT', ['DTSTAMP', 'UID', 'DTSTART', 'CLASS', 'CREATED',
        'DESCRIPTION', 'GEO', 'LAST-MODIFIED', 'LOCATION',
        'ORGANIZER', 'PRIORITY', 'SEQUENCE', 'STATUS',
        'SUMMARY', 'TRANSP', 'URL', 'RECURRENCE-ID',
        'RRULE', 'DTEND', 'DURATION', 'ATTACH', 'ATTENDEE',
        'CATEGORIES', 'COMMENT', 'CONTACT', 'EXDATE',
        'REQUEST-STATUS', 'RELATED-TO', 'RESOURCES', 'RDATE']],
    ['VTODO', ['DTSTAMP', 'UID', 'CLASS', 'COMPLETED', 'CREATED',
        'DESCRIPTION', 'DTSTART', 'GEO', 'LAST-MODIFIED',
        'LOCATION', 'ORGANIZER', 'PERCENT-COMPLETE', 'PRIORITY',
        'RECURRENCE-ID', 'SEQUENCE', 'STATUS', 'SUMMARY', 'URL',
        'RRULE', 'DUE', 'DURATION', 'ATTACH', 'ATTENDEE', 'CATEGORIES',
        'COMMENT', 'CONTACT', 'EXDATE', 'REQUEST-STATUS', 'RELATED-TO',
        'RESOURCES', 'RDATE',]],
    ['VJOURNAL', ['DTSTAMP', 'UID', 'CLASS', 'CREATED', 'DTSTART', 'LAST-MODIFIED',
        'ORGANIZER', 'RECURRENCE-ID', 'SEQUENCE', 'STATUS', 'SUMMARY',
        'URL', 'RRULE', 'ATTACH', 'ATTENDEE', 'CATEGORIES', 'COMMENT',
        'CONTACT', 'DESCRIPTION', 'EXDATE', 'RELATED-TO', 'RDATE',
        'REQUEST-STATUS']],
    ['VFREEBUSY', ['DTSTAMP', 'UID', 'CONTACT', 'DTSTART', 'DTEND',
        'ORGANIZER', 'URL', 'ATTENDEE', 'COMMENT', 'FREEBUSY',
        'REQUEST-STATUS']],
    ['VTIMEZONE', ['TZID', 'LAST-MODIFIED', 'TZURL']],
    ['VALARM', ['ACTION', 'TRIGGER', 'DURATION', 'REPEAT', 'ATTACH',
        'DESCRIPTION', 'SUMMARY', 'ATTENDEE']],
    ['STANDARD', ['DTSTART', 'TZOFFSETFROM', 'TZOFFSETTO', 'RRULE',
        'COMMENT', 'RDATE', 'TZNAME']],
])

valid_fields.set('DAYLIGHT', valid_fields.get('STANDARD')!);

/**
   All registered property types for VComponents.

   Note that only some of these are valid for each type of component (VCALENDAR,
   VEVENT, ...), and that they all support both iana and `x-` extensions.
 */
type known_ical_types
    = 'ACTION'
    | 'ATTACH'
    | 'ATTENDEE'
    | 'CALSCALE'
    | 'CATEGORIES'
    | 'CLASS'
    | 'COMMENT'
    | 'COMPLETED'
    | 'CONTACT'
    | 'CREATED'
    | 'DESCRIPTION'
    | 'DTEND'
    | 'DTSTAMP'
    | 'DTSTART'
    | 'DUE'
    | 'DURATION'
    | 'EXDATE'
    | 'FREEBUSY'
    | 'GEO'
    | 'LAST-MODIFIED'
    | 'LOCATION'
    | 'METHOD'
    | 'ORGANIZER'
    | 'PERCENT-COMPLETE'
    | 'PRIORITY'
    | 'PRODID'
    | 'RDATE'
    | 'RECURRENCE-ID'
    | 'RELATED-TO'
    | 'REPEAT'
    | 'REQUEST-STATUS'
    | 'RESOURCES'
    | 'RRULE'
    | 'SEQUENCE'
    | 'STATUS'
    | 'SUMMARY'
    | 'TRANSP'
    | 'TRIGGER'
    | 'TZID'
    | 'TZNAME'
    | 'TZOFFSETFROM'
    | 'TZOFFSETTO'
    | 'TZURL'
    | 'URL'
    | 'VERSION'

/**
 * Which types are valid to store under each property.
 * If multiple values are an option for that property, then
 * the list of possibilities will contain a sub-list (see example).
 *
 * ```json
 * { 'DTSTART': ['date', 'date-time'],
 *   'CATEGORIES': [['text']],
 *   ...
 * }
 * ```
 */
let valid_input_types: Map<string, Array<ical_type | ical_type[]>> =
    new Map([
        ['ACTION', ['text']], // AUDIO|DISPLAY|EMAIL|*other*
        ['ATTACH', ['uri', 'binary']],
        ['ATTENDEE', ['cal-address']],
        ['CALSCALE', ['text']],
        ['CATEGORIES', [['text']]],
        ['CLASS', ['text']], // PUBLIC|PRIVATE|CONFIDENTIAL|*other*
        ['COMMENT', ['text']],
        ['COMPLETED', ['date-time']],
        ['CONTACT', ['text']],
        ['CREATED', ['date-time']],
        ['DESCRIPTION', ['text']],
        ['DTEND', ['date', 'date-time']],
        ['DTSTAMP', ['date-time']],
        ['DTSTART', ['date', 'date-time']],
        ['DUE', ['date', 'date-time']],
        ['DURATION', ['duration']],
        ['EXDATE', [['date', 'date-time']]],
        ['EXRULE', []], /* deprecated */
        ['FREEBUSY', [['period']]],
        ['GEO', ['float']], // pair of floats
        ['LAST-MODIFIED', ['date-time']],
        ['LOCATION', ['text']],
        ['METHOD', ['text']],
        ['ORGANIZER', ['cal-address']],
        ['PERCENT-COMPLETE', ['integer']], // 0-100
        ['PRIORITY', ['integer']], // 0-9
        ['PRODID', ['text']],
        ['RDATE', [['date', 'date-time', 'period']]],
        ['RECURRENCE-ID', ['date', 'date-time']],
        ['RELATED-TO', ['text']],
        ['REPEAT', ['integer']],
        ['REQUEST-STATUS', ['text']],
        ['RESOURCES', [['text']]],
        ['RRULE', ['recur']],
        ['SEQUENCE', ['integer']],
        ['STATUS', ['text']], // see 3.8.1.11
        ['SUMMARY', ['text']],
        ['TRANSP', ['text']], // OPAQUE|TRANSPARENT
        ['TRIGGER', ['duration', 'date-time']],
        ['TZID', ['text']],
        ['TZNAME', ['text']],
        ['TZOFFSETFROM', ['utc-offset']],
        ['TZOFFSETTO', ['utc-offset']],
        ['TZURL', ['uri']],
        ['UID', ['text']],
        ['URL', ['uri']],
        ['VERSION', ['text']],
    ])

// type JCalLine {
// }

/** The UID of a VEvent, to make declarations clearer.  */
type uid = string

/* TODO is this type correct?
   What really are valid values for any? Does that depend on ical_type? Why is the tail a list?
   What really is the type for the parameter map?
*/
/* TODO link to RFC 7265 (jCal) */
/**
 * Alias for a record consisting of
 * - the name of the type, as a string
 * - all parameters of the object, as a `Record<String, any>`
 * - An `ical_type` value, noting the type of the final field(s)
 * - and one or more values of the type specified by the third field.
 */
type JCalProperty
    = [string, Record<string, any>, ical_type, any]
    | [string, Record<string, any>, ical_type, ...any[]]

/**
   Base type for JCal documents.

   Each VComponent in a JCal document is of this form.

   - The first element is the components type
     ('vevent', 'vcalendar', ...), in all lower case
   - The second element is is all properties directly
     on the component.
   - The third element is a list of all children.
*/
type JCal = [string, JCalProperty[], JCal[]]

/** The xml namespace name for xcalendar */
const xcal = "urn:ietf:params:xml:ns:icalendar-2.0";

/**
   An entry into a changelog.

   This is primarily used by VEvent, to track what has happened during a
   session.
 */
interface ChangeLogEntry {
    /**
       Type of change

       'property' is used for changes to properties.

       'calendar' is used when the containing calendar of a VEVENT is changed
    */
    type: 'calendar' | 'property',
    /** The name of the filed changed */
    name: string,
    /** The previous value, `null` if just created */
    from: string | null,
    /** The new value, `null` if removed */
    to: string | null,
}
