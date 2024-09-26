

function create_expand_link(body, display_name) {
    const link = document.createElement('a');
    const COLLAPSED_TEXT = "Show " + display_name;
    const EXPANDED_TEXT = "Hide " + display_name;

    link.href = '#';
    link.textContent = COLLAPSED_TEXT;

    link.addEventListener('click', event => {
        event.preventDefault();
        if (link.textContent == COLLAPSED_TEXT) {
            body.style.display = 'block';
            link.textContent = EXPANDED_TEXT;
        } else {
            body.style.display = 'none';
            link.textContent = COLLAPSED_TEXT;
        }
    });
    return link;
}

function capitalize(word) {
    return word.charAt(0).toUpperCase() + word.slice(1);
}

function add_collpsable(tag_name, display_name, display_name_capitalized) {
    let elements = document.getElementsByTagName(tag_name);
    for (var i = 0; i < elements.length; i++) {
        let element = elements[i];

        const children = element.children;
        element.children = [];
        const inner_body = document.createElement('collapsable_body');
        inner_body.style.display = 'none';
        const inner_text = document.createElement('b');
        inner_text.textContent = capitalize(display_name) + ".";
        inner_body.appendChild(inner_text);

        // Transfer children to inner_body
        while (element.firstChild) {
            inner_body.appendChild(element.firstChild);
        }
        const text = document.createElement('p');
        text.textContent = element.textContent;
        element.textContent = '';
        inner_body.appendChild(text);

        // We can now make the block visible since the main
        // child is hidden
        element.style.display = 'block';

        element.appendChild(inner_body);

        const expand_link = create_expand_link(inner_body, display_name);
        element.appendChild(expand_link);
    }
}

document.addEventListener('DOMContentLoaded', function() {
    add_collpsable("proof", "proof");
    add_collpsable("nonproof", "non-proof");
});
