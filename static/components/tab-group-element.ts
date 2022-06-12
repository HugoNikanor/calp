import { ComponentVEvent } from './vevent'
import { makeElement, gensym } from '../lib'
import { EditRRule } from './edit-rrule'
import { VEvent, isRedrawable } from '../vevent'
import { vcal_objects } from '../globals'

export { TabGroupElement }

/* Lacks a template, since it's trivial
   The initial children of this element all becomes tabs, each child may have
   the datapropertys 'label' and 'title' set, where label is what is shown in
   the tab bar, and title is the hower text.

   All additions and removals of tabs MUST go through addTab and removeTab!

   Information about how tabs should work from an accesability standpoint can be
   found here:
    https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Roles/Tab_Role

   <tab-group/>
*/
class TabGroupElement extends ComponentVEvent {

    readonly menu: HTMLElement;

    tabs: HTMLElement[] = [];
    tabLabels: HTMLElement[] = [];

    constructor(uid?: string) {
        super(uid);

        this.menu = makeElement('div', {}, {
            role: 'tablist',
            'aria-label': 'Simple Tabs',
        })
    }

    redraw(data: VEvent) {
        /* Update our tabset to match data:s having or not having of rrule,
           but do nothing if we already match */
        let rrule_tab = this.has_rrule_tab()
        if (data.getProperty('rrule')) {
            if (!this.has_rrule_tab()) {
                /* Note that EditRRule register itself to be updated on changes
                to the event */
                this.addTab(new EditRRule(data.getProperty('uid')),
                    "↺", "Upprepningar");
            }
        } else {
            if (rrule_tab) this.removeTab(rrule_tab as HTMLElement);
        }

        /* TODO is there any case where we want to propagate the draw to any of
           our tabs? or are all our tabs independent? */
    }

    connectedCallback() {
        /* All pre-added children should become tabs, but start with removing
        them and storing them for later */
        let originalChildren: HTMLElement[] = [];
        while (this.firstChild) {
            originalChildren.push(this.removeChild(this.firstChild) as HTMLElement);
        }

        /* Add our tab label menu */
        this.appendChild(this.menu);

        /* Re-add our initial children, but as proper tab elements */
        for (let child of originalChildren) {
            this.addTab(child);
        }

        /* redraw might add or remove tabs depending on our data, so call it here */
        this.redraw(vcal_objects.get(this.uid)!);

        /* All tabs should now be ready, focus the first one */
        if (this.tabLabels.length > 0) {
            this.tabLabels[0].setAttribute('tabindex', '0');
            this.tabLabels[0].click();
        }

    } /* end connectedCallback */

    addTab(child: HTMLElement, label?: string, title?: string) {

        /* First character of text is a good a guess as any for our label,
        but still defaut to '?' if no text is found */
        label = label || child.dataset.label || (child.textContent + '?')[0];
        title = title || child.dataset.title || '';
        let extra_attributes = {};
        /* Used to target a tab by name */
        if (child.dataset.originaltitle) {
            extra_attributes = { 'data-originaltitle': child.dataset.originaltitle }
        }

        let tab_id = gensym('tab_content_');
        let label_id = gensym('tab_label_');

        let tabLabel = makeElement('button', {
            textContent: label,
        }, {
            role: 'tab',
            id: label_id,
            tabindex: -1,
            title: title,
            'aria-selected': false,
            'aria-controls': tab_id,
            ...extra_attributes,
        })

        let tabContainer = makeElement('div', {}, {
            id: tab_id,
            role: 'tabpanel',
            tabindex: 0,
            hidden: 'hidden',
            'aria-labelledby': label_id,
        })

        tabContainer.replaceChildren(child);
        this.tabs.push(tabContainer);
        this.appendChild(tabContainer);

        this.tabLabels.push(tabLabel);
        this.menu.appendChild(tabLabel);

        tabLabel.addEventListener('click', () => this.tabClickedCallback(tabLabel));

        this.style.setProperty('--tabcount', '' + this.tabs.length);
    }

    removeTab(tab: HTMLElement) {
        let id = tab.getAttribute('aria-labelledby')!
        let label = document.getElementById(id)
        if (label) {
            if (label.ariaSelected === 'true') {
                this.tabLabels[0].click();
            }
            this.tabLabels = this.tabLabels.filter(el => el !== label)
            label.remove();
        }
        /* remove tab */
        this.tabs = this.tabs.filter(el => el !== tab)
        this.removeChild(tab);
        if (tab.firstChild) {
            let child = tab.firstChild as HTMLElement;
            if (isRedrawable(child)) {
                vcal_objects.get(this.uid)?.unregister(child)
            }
        }

        this.style.setProperty('--tabcount', '' + this.tabs.length);
    }

    /* TODO replace querySelectors here with our already saved references */
    tabClickedCallback(tab: Element) {

        /* hide all tab panels */
        for (let tabcontent of this.querySelectorAll('[role="tabpanel"]')) {
            tabcontent.setAttribute('hidden', 'true');
        }
        /* unselect all (selected) tab handles */
        for (let item of this.querySelectorAll('[aria-selected="true"]')) {
            item.setAttribute('aria-selected', 'false');
        }
        /* re-select ourselves */
        tab.setAttribute('aria-selected', 'true');

        /* unhide our target tab */
        this.querySelector('#' + tab.getAttribute('aria-controls'))!
            .removeAttribute('hidden')
    }


    /* returns our rrule tab if we have one */
    has_rrule_tab(): Element | false {
        for (let child of this.children) {
            if ((child.firstChild! as HTMLElement).tagName.toLowerCase() === 'vevent-edit-rrule') {
                return child;
            }
        }
        return false;
    }

}
