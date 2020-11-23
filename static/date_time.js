function init_date_time() {
    for (let dt of document.getElementsByClassName("date-time")) {
        dt.time = dt.querySelector('[type=time]');
        dt.date = dt.querySelector('[type=date]');

        Object.defineProperty(dt, 'value', {
            get: () => (dt.date.value && dt.time.value)
            // TODO wrapping <date-time/> tag
                ? dt.date.value + "T" + dt.time.value
                : "",
            set: (v) => [dt.date.value, dt.time.value] = v.split("T"),
        });

        Object.defineProperty(dt, 'name', {
            get: () => dt.attributes.name.value
        });

        dt._addEventListener = dt.addEventListener;
        dt.addEventListener = function (field, proc) {
            switch (field) {
            case 'input':
                dt.time.addEventListener(field, proc);
                dt.date.addEventListener(field, proc);
                break;
            default:
                dt._addEventListener(field, proc);
            }
        }
    }
}
