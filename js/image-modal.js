(() => {
  // Preserve the original-image link when modal dialogs are unavailable.
  if (typeof HTMLDialogElement === 'undefined' ||
      typeof HTMLDialogElement.prototype.showModal !== 'function') return;

  let dialog, image, original, trigger, previousOverflow, openedWithKeyboard;

  function createDialog() {
    dialog = document.createElement('dialog');
    dialog.className = 'image-modal';
    dialog.setAttribute('aria-label', 'Expanded image');
    const controls = document.createElement('div');
    controls.className = 'image-modal__controls';
    original = document.createElement('a');
    original.textContent = 'Open image in new tab';
    original.target = '_blank';
    original.rel = 'noopener noreferrer';
    const close = document.createElement('button');
    close.type = 'button';
    close.textContent = '×';
    close.setAttribute('aria-label', 'Close');
    close.title = 'Close';
    close.autofocus = true;
    close.addEventListener('click', () => dialog.close());
    controls.append(original, close);
    image = document.createElement('img');
    image.className = 'image-modal__image';
    dialog.append(controls, image);
    document.body.append(dialog);

    dialog.addEventListener('click', (event) => {
      const rect = dialog.getBoundingClientRect();
      if (event.target === dialog &&
          (event.clientX < rect.left || event.clientX > rect.right ||
           event.clientY < rect.top || event.clientY > rect.bottom)) {
        dialog.close();
      }
    });
    dialog.addEventListener('keydown', (event) => {
      if (event.key !== 'Tab') return;
      if (event.shiftKey && document.activeElement === original) {
        event.preventDefault();
        close.focus();
      } else if (!event.shiftKey && document.activeElement === close) {
        event.preventDefault();
        original.focus();
      }
    });
    // Native dialog provides Escape handling and makes the background inert.
    dialog.addEventListener('close', () => {
      // A queued close event may arrive after another thumbnail reopened it.
      if (dialog.open) return;
      document.body.style.overflow = previousOverflow;
      previousOverflow = undefined;
      image.removeAttribute('src');
      if (trigger && trigger.isConnected) {
        // Restore keyboard focus without leaving a ring on mouse-opened images.
        if (openedWithKeyboard) trigger.focus({ preventScroll: true });
        else trigger.blur();
      }
    });
  }

  document.addEventListener('click', (event) => {
    if (!(event.target instanceof Element)) return;
    const link = event.target.closest('a[data-image-modal]');
    if (!link || event.button !== 0 || event.ctrlKey || event.metaKey ||
        event.shiftKey || event.altKey) return;
    event.preventDefault();
    if (!dialog) createDialog();
    trigger = link;
    openedWithKeyboard = event.detail === 0;
    image.alt = link.querySelector('img').alt;
    image.src = link.href;
    original.href = link.href;
    if (previousOverflow === undefined) previousOverflow = document.body.style.overflow;
    dialog.showModal();
    document.body.style.overflow = 'hidden';
  });
})();
