'use strict';
/*
  General procedures which in theory could be used anywhere.
 */

/* ----- Date Extensions ---------------------------- */

/*
  Extensions to Javascript's Date to allow representing times
  with different timezones. Currently only UTC and local time
  are supported, but more should be able to be added.

  NOTE that only the raw `get' (and NOT the `getUTC') methods
  should be used on these objects, and that the reported timezone
  is quite often wrong.
 */

function parseDate(str) {
    let year, month, day, hour=false, minute, second=0, utc;

    let end = str.length - 1;
    if (str[end] == 'Z') {
        utc = true;
        str = str.substring(0, end);
    };

    switch (str.length) {
    case '2020-01-01T13:37:00'.length:
        second = str.substr(17,2);
    case '2020-01-01T13:37'.length:
        hour = str.substr(11,2);
        minute = str.substr(14,2);
    case '2020-01-01'.length:
        year = str.substr(0,4);
        month = str.substr(5,2) - 1;
        day = str.substr(8,2);
        break;
    default:
        throw 'Bad argument';
    }

    let date;
    if (hour) {
        date = new Date(year, month, day, hour, minute, second);
        date.utc = utc;
        date.dateonly = false;
    } else {
        date = new Date(year, month, day);
        date.dateonly = true;
    }
    return date;
}

function copyDate(date) {
    let d = new Date(date);
    d.utc = date.utc;
    d.dateonly = date.dateonly;
    return d;
}

function to_local(date) {
    if (! date.utc) return date;

    return new Date(date.getTime() - date.getTimezoneOffset() * 60 * 1000);
}

/* -------------------------------------------------- */

function makeElement (name, attr={}) {
    let element = document.createElement(name);
    for (let [key, value] of Object.entries(attr)) {
        element[key] = value;
    }
    return element;
}

function round_time (time, fraction) {
    let scale = 1 / fraction;
    return Math.round (time * scale) / scale;
}

/* only used by the bar.
   Events use the start and end time of their container, but since the bar
   is moving between containers that is clumsy.
   Just doing (new Date()/(86400*1000)) would be nice, but there's no good
   way to get the time in the current day.
 */
function date_to_percent (date) {
    return (date.getHours() + (date.getMinutes() / 60)) * 100/24;
}

/* if only there was such a thing as a let to wrap around my lambda... */
/* js infix to not collide with stuff generated backend */
const gensym = (counter => (prefix="gensym") => prefix + "js" + ++counter)(0)

function setVar(str, val) {
	document.documentElement.style.setProperty("--" + str, val);
}



function datepad(thing, width=2) {
    return (thing + "").padStart(width, "0");
}

function format_date(date, str) {
    let fmtmode = false;
    let outstr = "";
    for (var i = 0; i < str.length; i++) {
        if (fmtmode) {
            switch (str[i]) {
                /* Moves the date into local time. */
            case 'L': date = to_local(date); break;
            case 'Y': outstr += datepad(date.getFullYear(), 4); break;
            case 'm': outstr += datepad(date.getMonth() + 1);   break;
            case 'd': outstr += datepad(date.getDate());        break;
            case 'H': outstr += datepad(date.getHours());       break;
            case 'M': outstr += datepad(date.getMinutes());     break;
            case 'S': outstr += datepad(date.getSeconds());     break;
            case 'Z': if (date.utc) outstr += 'Z'; break;
            }
            fmtmode = false;
        } else if (str[i] == '~') {
            fmtmode = true;
        } else {
            outstr += str[i];
        }
    }
    return outstr;
}
Object.prototype.format = function () { return this; } /* any number of arguments */
Date.prototype.format = function (str) { return format_date (this, str); }
