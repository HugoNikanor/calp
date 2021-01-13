

/* event component => coresponding popup component */
function event_from_popup(popup) {
    return document.getElementById(popup.id.substr(5))
}

/* popup component => coresponding event component */
function popup_from_event(event) {
    return document.getElementById("popup" + event.id);
}

/* hides given popup */
function close_popup(popup) {
    popup.classList.remove("visible");
}

/* hides all popups */
function close_all_popups () {
    for (let popup of document.querySelectorAll(".popup-container.visible")) {
        close_popup(popup);
    }
}

/* open given popup */
function open_popup(popup) {
    popup.classList.add("visible");
    let element = event_from_popup(popup);
    // let root = document.body;
    let root;
    switch (VIEW) {
    case 'week':
        root = document.getElementsByClassName("days")[0];
        break;
    case 'month':
    default:
        root = document.body;
        break;
    }
    /* start <X, Y> sets offset between top left corner
       of event in calendar and popup. 10, 10 soo old
       event is still visible */
    let offsetX = 10, offsetY = 10;
    while (element !== root) {
        offsetX += element.offsetLeft;
        offsetY += element.offsetTop;
        element = element.offsetParent;
    }
    popup.style.left = offsetX + "px";
    popup.style.top = offsetY + "px";
}

/* toggles open/closed status of popup given by id */
function toggle_popup(popup_id) {
    let popup = document.getElementById(popup_id);
    if (popup.classList.contains("visible")) {
        close_popup(popup);
    } else {
        open_popup(popup);
    }
}

/* Code for managing "selected" popup */
/* Makes the popup last hovered over the selected popup, moving it to
 * the top, and allowing global keyboard bindings to affect it. */

let activePopup;

for (let popup of document.querySelectorAll('.popup-container')) {
    /* TODO possibly only change "active" element after a fraction of
     * a second, for example when moving between tabs */
    popup.addEventListener('mouseover', function () {
        /* This is ever so slightly inefficient,
           but it really dosen't mammet */
        for (let other of
             document.querySelectorAll('.popup-container'))
        {
            /* TODO get this from somewhere */
            /* Currently it's manually copied from the stylesheet */
            other.style['z-index'] = 1000;
        }
        popup.style['z-index'] += 1;
        activePopup = popup;
    });
}

document.addEventListener('keydown', function (event) {
    /* Physical key position, names are what that key would
       be in QWERTY */
    let i = ({
            'KeyQ': 0,
            'KeyW': 1,
            'KeyE': 2,
            'KeyR': 3,
    })[event.code];
    if (i === undefined) return
    if (! activePopup) return;
    let element = activePopup.querySelectorAll(".tab > label")[i];
    if (! element) return;
    element.click();
});

/* END Code for managing "selected" popup */
