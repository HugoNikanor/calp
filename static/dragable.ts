export { bind_popup_control }

/*
  Apply to a given component to make it draggable.
  Drag area (usually a title bar) should be be the only argument.
  It is REQUIRED that the object which should be moved have the class
  'popup-container';
*/


/*
  Given the navbar of a popup, make it dragable.
 */
function bind_popup_control(nav: HTMLElement) {

    // if (!nav.closest('popup-element')) {
    //     console.log(nav);
    //     throw TypeError('not a popup container');
    // }

    nav.onmousedown = function(e) {
        /* Ignore mousedown on children */
        if (e.target != nav) return;
        nav.style.cursor = "grabbing";
        nav.dataset.grabbed = "true";
        nav.dataset.grabPoint = e.clientX + ";" + e.clientY;
        // let popup = nav.closest(".popup-container");
        let popup = nav.closest("popup-element") as HTMLElement;
        nav.dataset.startPoint = popup.offsetLeft + ";" + popup.offsetTop;
    }
    window.addEventListener('mousemove', function(e) {
        if (nav.dataset.grabbed) {
            let [x, y] = nav.dataset.grabPoint!.split(";").map(Number);
            let [startX, startY] = nav.dataset.startPoint!.split(";").map(Number);
            // let popup = nav.closest(".popup-container");
            let popup = nav.closest("popup-element") as HTMLElement;

            popup.style.left = startX + (e.clientX - x) + "px";
            popup.style.top = startY + (e.clientY - y) + "px";
        }
    });
    window.addEventListener('mouseup', function() {
        nav.dataset.grabbed = "";
        nav.style.cursor = "";
    });
}
