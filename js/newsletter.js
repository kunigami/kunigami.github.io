(() => {
  const form = document.getElementById('newsletter-form');
  const button = form.querySelector('button');
  const status = document.getElementById('newsletter-status');
  button.disabled = false;

  form.addEventListener('submit', async (event) => {
    event.preventDefault();
    if (button.disabled) return;

    button.disabled = true;
    status.textContent = 'Subscribing…';

    try {
      const response = await fetch('https://newsletter.kuniga.me/subscribe', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ email: form.elements.email.value.trim() }),
      });
      if (!response.ok) throw new Error('Subscription failed');

      status.textContent = 'Check your email to confirm your subscription.';
      form.reset();
    } catch (error) {
      status.textContent = 'Unable to subscribe. Please try again in a moment.';
    } finally {
      button.disabled = false;
    }
  });
})();
