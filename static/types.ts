export {
    all_types, ical_type, property_names,
    valid_fields, known_ical_types,
    valid_input_types, tagname,
    JCalProperty, JCal
}

let all_types = [
    'text',
    'uri',
    'binary',
    'float',        /* Number.type = 'float' */
    'integer',      /* Number.type = 'integer' */
    'date-time',    /* Date */
    'date',         /* Date.dateonly = true */
    'duration',
    'period',
    'utc-offset',
    'cal-address',
    'recur',        /* RRule */
    'boolean',      /* boolean */
]

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

let property_names = [
    'calscale', 'method', 'prodid', 'version', 'attach', 'categories',
    'class', 'comment', 'description', 'geo', 'location', 'percent-complete',
    'priority', 'resources', 'status', 'summary', 'completed', 'dtend', 'due',
    'dtstart', 'duration', 'freebusy', 'transp', 'tzid', 'tzname', 'tzoffsetfrom',
    'tzoffsetto', 'tzurl', 'attendee', 'contact', 'organizer', 'recurrence-id',
    'related-to', 'url', 'uid', 'exdate', 'exrule', 'rdate', 'rrule', 'action',
    'repeat', 'trigger', 'created', 'dtstamp', 'last-modified', 'sequence', 'request-status'
];


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

let valid_input_types: Map<string, ical_type | ical_type[]> =
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
        ['URL', ['uri']],
        ['VERSION', ['text']],
    ]) as Map<string, ical_type | ical_type[]>

// type JCalLine {
// }

type tagname = 'vevent' | string

type JCalProperty = [string, any, ical_type, any[]]

type JCal = [tagname, JCalProperty[], JCal[]]
