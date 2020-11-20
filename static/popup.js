

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
