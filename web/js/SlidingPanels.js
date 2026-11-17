// Shared nonmodal drawers. Each side keeps one panel open; narrow maps use one
// overall, so the map and other edge tabs cannot be trapped behind overlapping panels.
let openSequence = 0;

export class SlidingPanel {
    constructor(host, registry) {
        this.host = host;
        this.root = host.parentElement;
        this.registry = registry;
        this.side = host.dataset.side;
        this.panel = host.querySelector('.sliding-panel');
        this.toggle = host.querySelector('.drawer-tab');
        this.closeButton = host.querySelector('.drawer-close');
        this.title = host.querySelector('h2').textContent;
        this.isOpen = false;
        this.setOpen(false);
        this.toggle.addEventListener('click', () => {
            this.setOpen(!this.isOpen);
            // The drawer is still offscreen at the start of its animation.
            // Normal focus would scroll overflow:hidden ancestors to reveal it.
            if (this.isOpen) this.closeButton.focus({preventScroll: true});
        });
        this.closeButton.addEventListener('click', () => this.setOpen(false));
        this.panel.addEventListener('keydown', event => {
            if (event.key === 'Escape') {
                event.preventDefault();
                event.stopPropagation();
                this.setOpen(false);
            }
        });
    }

    setOpen(open) {
        if (open) {
            for (const other of this.registry.values()) {
                if (other !== this && other.isOpen
                    && (other.side === this.side || this.root.clientWidth < 760)) other.setOpen(false);
            }
        }
        if (!open && this.panel.contains(document.activeElement)) this.toggle.focus({preventScroll: true});
        this.isOpen = open;
        if (open) this.openSequence = ++openSequence;
        this.panel.inert = !open;
        this.panel.setAttribute('aria-hidden', String(!open));
        this.toggle.setAttribute('aria-expanded', String(open));
        this.toggle.setAttribute('aria-label', `${open ? 'Close' : 'Open'} ${this.title}`);
        this.host.classList.toggle('drawer-open', open);
        this.root.classList.toggle(`${this.side}-drawer-open`, open
            || [...this.registry.values()].some(other => other.side === this.side && other.isOpen));
    }

    open() {
        this.setOpen(true);
    }
}

export default function setupSlidingPanels(root) {
    const panels = new Map();
    for (const host of root.querySelectorAll('.drawer')) {
        panels.set(host.dataset.panel, new SlidingPanel(host, panels));
    }
    for (const panel of panels.values()) {
        if (panel.host.hasAttribute('data-open-default')) panel.open();
    }
    root.ownerDocument?.defaultView?.addEventListener('resize', () => {
        if (root.clientWidth >= 760) return;
        const opened = [...panels.values()].filter(panel => panel.isOpen)
            .sort((a, b) => b.openSequence - a.openSequence);
        for (const panel of opened.slice(1)) panel.setOpen(false);
    });
    return panels;
}
