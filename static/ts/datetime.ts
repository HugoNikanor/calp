/**
  Extensions to Javascript's Date to allow representing times
  with different timezones. Currently only UTC and local time
  are supported, but more should be able to be added.

  NOTE that only the raw `get' (and NOT the `getUTC') methods
  should be used on these objects, and that the reported timezone
  is quite often wrong.

  TODO The years between 0 and 100 (inclusive) gives dates in the twentieth
  century, due to how javascript works (...).
 */
export {
    parse_date,
    format_date,
    to_local,
}

declare global {
    interface Date {
        /** Indicates if the date object is in UTC or local time. */
        utc: boolean

        /**
           Indicates that the object only contains a date component.

           This means that any time is ignored in most operations.
        */
        dateonly: boolean
    }
}

/**
 * Takes a string `str`, which should be in ISO-8601 date-format, and
 * returns a javascript Date object.
 * Handles date-times, with and without seconds, trailing `Z' for
 * time-zones, and dates without times.
 * If no time is given the `dateonly` attribute is set to yes.
 */
function parse_date(str: string): Date {
    let year: number;
    let month: number;
    let day: number;
    let hour: number | false = false;
    let minute: number = 0;
    let second: number = 0;
    let utc: boolean = false;

    const end = str.length - 1;
    if (str[end] == 'Z') {
        utc = true;
        str = str.substring(0, end);
    };

    switch (str.length) {
        case '2020-01-01T13:37:00'.length:
            second = +str.substr(17, 2);
        /* fallthrough */
        case '2020-01-01T13:37'.length:
            hour = +str.substr(11, 2);
            minute = +str.substr(14, 2);
        /* fallthrough */
        case '2020-01-01'.length:
            year = +str.substr(0, 4);
            month = +str.substr(5, 2) - 1;
            day = +str.substr(8, 2);
            break;
        default:
            throw `"${str}" doesn't look like a date/-time string`
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

/**
 * Returns a Date object (which may be new) which is guaranteed in local time.
 * This means that the `utc` field is `false`, and that
 * `to_local(current_time())` should show what your wall-clock shows.
 */
function to_local(date: Date): Date {
    if (!date.utc) return date;

    return new Date(date.getTime() - date.getTimezoneOffset() * 60 * 1000);
}

function datepad(thing: number | string, width = 2): string {
    return (thing + "").padStart(width, "0");
}

/**
   Format a date into a string.

   @param date
   The datetime to format

   @param format
   How the date should be converted into a string.

   The format is similar to `strftime`, but with tilde (`~`) characters
   instead of percent signs, to match how Scheme does it. Valid format
   specifiers are:

   | Fmt  |               Output             | Width¹ |
   |------|----------------------------------|--------|
   | `~~` | Literal '~'                      |        |
   | `~Y` | Year                             | 4      |
   | `~m` | Month number                     | 2      |
   | `~d` | Day of month                     | 2      |
   | `~H` | Hour                             | 2      |
   | `~M` | Minute                           | 2      |
   | `~S` | Second                           | 2      |
   | `~Z` | 'Z' if date is UTC, otherwise '' |        |
   | `~L` | Converts date to local time²     |        |

   - ¹ These fields will be left padded with zeroes to that width
   - ² This forces the output to be in local time, possibly converting
     timezone if needed. It then outputs nothing.
     See {@link to_local `to_local`} for details.

*/
function format_date(date: Date, format: string): string {
    let fmtmode = false;
    let outstr = "";
    for (let i = 0; i < format.length; i++) {
        if (fmtmode) {
            switch (format[i]) {
                /* Moves the date into local time. */
                case 'L': date = to_local(date); break;
                case 'Y': outstr += datepad(date.getFullYear(), 4); break;
                case 'm': outstr += datepad(date.getMonth() + 1); break;
                case 'd': outstr += datepad(date.getDate()); break;
                case 'H': outstr += datepad(date.getHours()); break;
                case 'M': outstr += datepad(date.getMinutes()); break;
                case 'S': outstr += datepad(date.getSeconds()); break;
                case 'Z': if (date.utc) outstr += 'Z'; break;
            }
            fmtmode = false;
        } else if (format[i] == '~') {
            fmtmode = true;
        } else {
            outstr += format[i];
        }
    }
    return outstr;
}
